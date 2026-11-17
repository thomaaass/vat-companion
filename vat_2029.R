# VAT base-broadening trade-off, 2029-30
# Bar width = consumption base (£bn), bar height = VAT rate, so area = revenue.
# Static: assumes nobody changes what they buy.

library(ggplot2)


# 1. HMRC historic relief costs (£m) ---------------------------------------
# Source: HMRC, Tax relief statistics (January 2026). 2025-26 is an HMRC estimate.
# For reference only; the plot uses the IFS 2029-30 figures below.

hmrc <- data.frame(
  relief  = c("Food", "New dwellings", "Domestic passenger transport",
              "Prescriptions", "Domestic fuel and power (5%)",
              "Water and sewerage", "VAT registration threshold"),
  y2020_21 = c(20600, 14500, 2100, 3300, 4900, 2400, 2600),
  y2021_22 = c(21000, 15900, 3500, 3300, 5400, 2400, 3200),
  y2022_23 = c(23800, 16600, 5700, 3500, 8800, 2400, 2900),
  y2023_24 = c(26100, 14500, 5200, 3600, 7600, 2700, 2900),
  y2024_25 = c(26300, 14400, 5600, 3800, 6800, 2900, 3000),
  y2025_26 = c(27400, 14600, 5800, 4000, 7000, 3700, 3100)
)
print(hmrc)


# 2. IFS forecasts for 2029-30 (£bn) ---------------------------------------
# Source: IFS Green Budget 2025, "Options for tax increases", section 4.2 and table 4.1.

yield_1pp <- 9.9    # revenue from 1pp on the 20% standard rate
total_zr  <- 83.1   # total cost of all zero and reduced rates

ifs <- data.frame(
  category = c("Domestic energy", "Food", "New homes", "Passenger transport",
               "Prescriptions", "Children's clothing"),
  cost     = c(7.8, 30.6, 19.0, 6.1, 4.4, 2.6),
  rate     = c(5, 0, 0, 0, 0, 0)
)
# Everything else (water, books, etc.) is the gap to the IFS total
ifs <- rbind(ifs, data.frame(category = "Other zero-rated",
                             cost = total_zr - sum(ifs$cost), rate = 0))


# 3. Turn costs into consumption bases -------------------------------------
# A relief's cost is (20% - its rate) x base, so base = cost / (20% - rate).
# The standard-rated base is 1pp yield / 1%.

std_rate <- 20
new_rate <- 18

d <- rbind(
  data.frame(category = "Standard-rated", cost = NA, rate = std_rate),
  ifs
)
d$base <- ifelse(d$category == "Standard-rated",
                 yield_1pp / 0.01,
                 d$cost / ((std_rate - d$rate) / 100))
d$xmax <- cumsum(d$base)
d$xmin <- d$xmax - d$base


# 4. Revenue sums ----------------------------------------------------------

revenue_now     <- sum(d$base * d$rate / 100)
neutral_rate    <- 100 * revenue_now / sum(d$base)
given_up        <- d$base[1] * (std_rate - new_rate) / 100
new_revenue     <- sum(d$base[-1] * (new_rate - d$rate[-1]) / 100)
net_raised      <- new_revenue - given_up

cat(sprintf("Revenue-neutral rate: %.1f%%\n", neutral_rate))
cat(sprintf("At %d%%: given up £%.1fbn, new £%.1fbn, net £%.1fbn\n",
            new_rate, given_up, new_revenue, net_raised))


# 5. Plot ------------------------------------------------------------------

bg <- "#FAF5EC"; ink <- "#1A1A1A"; grey <- "#6E6E6E"
col_kept <- "#DCD6C7"; col_given <- "#A3223F"; col_new <- "#1F4A3A"
W <- max(d$xmax)
std <- d[1, ]
oth <- d[-1, ]

# Shaded areas
kept  <- data.frame(xmin = d$xmin, xmax = d$xmax, ymin = 0,
                    ymax = pmin(d$rate, new_rate))
areas <- rbind(
  data.frame(xmin = std$xmin, xmax = std$xmax, ymin = new_rate, ymax = std_rate,
             what = "Revenue given up"),
  data.frame(xmin = oth$xmin, xmax = oth$xmax, ymin = oth$rate, ymax = new_rate,
             what = "New revenue")
)
areas$what <- factor(areas$what, levels = c("New revenue", "Revenue given up"))

# Outline of today's rates
step <- data.frame(x = c(0, rbind(d$xmin, d$xmax), W),
                   y = c(0, rep(d$rate, each = 2), 0))

# Category labels: standard-rated centred, the rest fanned out with leader lines
oth$mid   <- (oth$xmin + oth$xmax) / 2
oth$label_x <- seq(min(oth$xmin), W * 1.04, length.out = nrow(oth) + 1)[-1] -
  (W * 1.04 - min(oth$xmin)) / (2 * nrow(oth))

p <- ggplot() +
  geom_hline(yintercept = c(5, 10, 15, 20), colour = "#E2DED2", linewidth = 0.35) +
  geom_rect(data = kept, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = col_kept) +
  geom_rect(data = areas, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                              fill = what)) +
  geom_segment(data = oth, aes(x = xmin, xend = xmin, y = 0, yend = new_rate),
               colour = bg, linewidth = 0.5) +
  geom_path(data = step, aes(x, y), colour = ink, linewidth = 0.45) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.5) +
  # 18% line and revenue-neutral line
  annotate("segment", x = 0, xend = W, y = new_rate, yend = new_rate,
           colour = ink, linewidth = 0.5) +
  annotate("text", x = W, y = new_rate, label = paste0(new_rate, "% standard rate"),
           hjust = -0.05, size = 3.5, colour = ink) +
  annotate("segment", x = 0, xend = W, y = neutral_rate, yend = neutral_rate,
           colour = ink, linewidth = 0.5, linetype = "22") +
  annotate("text", x = W, y = neutral_rate, label = "Revenue neutral",
           hjust = -0.05, size = 3.5, colour = ink) +
  # £ labels inside the areas
  annotate("text", x = (std$xmin + std$xmax) / 2, y = (new_rate + std_rate) / 2,
           label = sprintf("£%.0fbn", given_up), colour = "white", size = 3.6) +
  annotate("text", x = mean(unlist(d[d$category == "Food", c("xmin", "xmax")])),
           y = new_rate / 2, label = sprintf("£%.0fbn", new_revenue),
           colour = "white", size = 3.6) +
  # Category labels
  annotate("text", x = (std$xmin + std$xmax) / 2, y = -0.7, label = std$category,
           vjust = 1, size = 3.4, colour = ink) +
  geom_segment(data = oth, aes(x = mid, xend = label_x, y = 0, yend = -1.4),
               colour = "#9C978B", linewidth = 0.3) +
  geom_text(data = oth, aes(x = label_x, y = -1.6, label = category),
            angle = 45, hjust = 1, vjust = 1, size = 3.1, colour = ink) +
  scale_fill_manual(values = c("New revenue" = col_new,
                               "Revenue given up" = col_given), name = NULL) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20),
                     labels = c("0", "5", "10", "15", "20%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, W), ylim = c(0, std_rate + 0.6), clip = "off") +
  labs(
    title = sprintf(
      "Ending VAT zero-rating with an %d%% standard rate would raise £%.0fbn in 2029-30",
      new_rate, net_raised),
    subtitle = "UK VAT rate by category. Bar width is the consumption base, so area is revenue",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", colour = ink, size = 17),
    plot.subtitle = element_text(colour = ink, size = 12, margin = margin(b = 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 6, -27),
    axis.text.y = element_text(colour = grey, size = 10.5),
    plot.margin = margin(16, 125, 95, 16)
  )

ggsave("vat_2029.png", p, width = 10, height = 6.2, dpi = 200)


# ==========================================================================
# PART 2: Who pays? Change in VAT per household, by income decile
# ==========================================================================
# Back-of-envelope, using ONS household spending patterns for 2024-25.
# Assumes VAT changes are passed fully into prices and households don't
# change what they buy.


# 6. ONS spending data (2024-25) -------------------------------------------
# Source: ONS, Family spending in the UK, April 2024 to March 2025 (June 2026);
# decile food and energy shares via House of Commons Library (Sept 2026).

ons_mean_spend   <- 676.60   # average weekly household spending, £
ons_q1_spend     <- 407.30   # poorest fifth, £/week
ons_q5_spend     <- 1083.60  # richest fifth, £/week
ons_mean_food    <- 73.70    # average weekly food and non-alcoholic drink, £

ons_food_share   <- c(d1 = 15.6, d2 = 14.8, d10 = 7.1)  # % of spending
ons_energy_share <- c(d1 = 8.5, d10 = 3.7)             # % of spending


# 7. Assumptions (where ONS figures weren't available) ---------------------

# Weekly spending by decile: interpolated so deciles 1-2 average the poorest
# fifth, 9-10 the richest fifth, and all ten the overall mean.
spend <- c(370, 445, 495, 545, 595, 650, 715, 784, 930, 1237)

# Food share: ONS values for deciles 1, 2 and 10, smooth in between;
# then scaled so average food spend matches the ONS mean.
food_share <- c(15.6, 14.8, 13.8, 12.9, 12.0, 11.2, 10.4, 9.6, 8.6, 7.1) / 100

# Energy share: straight line between the ONS values for deciles 1 and 10.
energy_share <- seq(8.5, 3.7, length.out = 10) / 100

# Other zero-rated (fares, children's clothes, books): 4.5% of spending,
# plus water at a flat £9/week.
other_share <- 0.045
water_week  <- 9

# Standard-rated share of spending: 42% for the poorest, 56% for the richest.
std_share <- seq(0.42, 0.56, length.out = 10)


# 8. Calculate the change in VAT per household (£/year) --------------------

food   <- food_share * spend
food   <- food * ons_mean_food / mean(food)
energy <- energy_share * spend
other  <- other_share * spend + water_week
std    <- std_share * spend

# Price changes from the reform
p_zero   <- new_rate / 100                      # 0%  -> 18%
p_energy <- (1 + new_rate / 100) / 1.05 - 1     # 5%  -> 18%
p_std    <- (1 + new_rate / 100) / 1.20 - 1     # 20% -> 18% (negative)

dist <- data.frame(
  decile = factor(1:10),
  Food   = food   * p_zero   * 52,
  Energy = energy * p_energy * 52,
  `Other zero-rated` = other * p_zero * 52,
  `Cheaper standard-rated goods` = std * p_std * 52,
  check.names = FALSE
)
dist$net       <- rowSums(dist[, 2:5])
dist$gross     <- rowSums(dist[, 2:4])
dist$pct_spend <- 100 * dist$net / (spend * 52)
print(round(dist[, c("Food", "Energy", "Other zero-rated", "Cheaper standard-rated goods", "net", "pct_spend")], 1))


# 9. Plot ------------------------------------------------------------------

long <- data.frame(
  decile    = rep(dist$decile, 4),
  component = rep(names(dist)[2:5], each = 10),
  value     = unlist(dist[, 2:5])
)
long$component <- factor(long$component, levels = names(dist)[2:5])

q <- ggplot() +
  geom_hline(yintercept = seq(-500, 2000, 500), colour = "#E2DED2", linewidth = 0.35) +
  geom_col(data = long, aes(x = decile, y = value, fill = component), width = 0.7) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.5) +
  geom_point(data = dist, aes(x = decile, y = net), size = 3, shape = 21,
             fill = ink, colour = "white", stroke = 0.8) +
  geom_text(data = dist, aes(x = decile, y = gross + 40,
                             label = sprintf("£%s\n%.1f%%", formatC(round(net, -1), format = "d", big.mark = ","), pct_spend)),
            vjust = 0, size = 3.1, colour = ink, lineheight = 0.9) +
  scale_fill_manual(values = c("Food" = col_new, "Energy" = "#4F7F6B",
                               "Other zero-rated" = "#9DBFAF",
                               "Cheaper standard-rated goods" = col_given),
                    name = NULL) +
  scale_y_continuous(breaks = seq(-500, 2000, 500), limits = c(-650, 2050),
                     labels = function(v) ifelse(v == 2000, "£2,000",
                                                 format(v, big.mark = ","))) +
  labs(
    title = sprintf(
      "The poorest households would pay %.1f%% more of their spending, the richest %.1f%%",
      dist$pct_spend[1], dist$pct_spend[10]),
    subtitle = sprintf(
      "Change in VAT paid per household per year (£), by income decile, with zero-rating ended and a %d%% standard rate.\nDots and labels show the net change (£ and %% of spending). 2024-25 spending patterns",
      new_rate),
    x = "Household income decile (1 = poorest)", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", colour = ink, size = 15),
    plot.subtitle = element_text(colour = ink, size = 11, margin = margin(b = 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 6, -40),
    axis.text   = element_text(colour = grey, size = 10.5),
    axis.title.x = element_text(colour = grey, size = 10.5, margin = margin(t = 8)),
    plot.margin = margin(16, 24, 12, 16)
  )

ggsave("vat_2029_deciles.png", q, width = 10, height = 6.2, dpi = 200)


# ==========================================================================
# PART 3: £1,000 per adult minus the household VAT cost in 2029-30
# ==========================================================================
# Parts 3-5 retain the earlier annual-payment illustrations for reference.
# The companion page pays once over two years; see Parts 7 and 9.
# ONS Family Spending 2024-25, workbook 1, table 3.1E, columns E:N:
# rows 18 (sample households) and 20 (sample adults).
# Equivalised disposable income deciles, updated two-adult reference scale;
# this matches the food-share benchmarks used above.
# https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/expenditure/datasets/familyspendingworkbook1detailedexpenditureandtrends
# Adult counts are estimated from rounded, unweighted sample counts.

# OBR October 2026, table A.3: compound CPI from 2024-25 to 2029-30.
cpi_years <- c("2025-26", "2026-27", "2027-28", "2028-29", "2029-30")
cpi_rates <- c(3.4, 3.3, 3.1, 2.0, 2.1)
inflation_factor_2029 <- prod(1 + cpi_rates / 100)

sample_households <- c(500, 480, 520, 490, 530, 510, 510, 520, 490, 460)
sample_adults     <- c(710, 720, 850, 870, 990, 990, 1010, 1000, 940, 900)
payment_per_adult <- 1000

compensation <- data.frame(
  decile = dist$decile,
  sample_households = sample_households,
  sample_adults = sample_adults,
  adults_per_household = sample_adults / sample_households,
  vat_cost_2024 = dist$net
)
compensation$payment <- compensation$adults_per_household * payment_per_adult
compensation$vat_cost_2029 <- compensation$vat_cost_2024 * inflation_factor_2029
compensation$balance <- compensation$payment - compensation$vat_cost_2029

signed_pounds <- function(x) {
  paste0(ifelse(x < 0, "\u2212", ifelse(x > 0, "+", "")), "£",
         formatC(abs(round(x)), format = "d", big.mark = ","))
}

r <- ggplot(compensation, aes(x = decile, y = balance)) +
  geom_hline(yintercept = pretty(c(0, compensation$balance), n = 5),
             colour = "#E2DED2", linewidth = 0.35) +
  geom_col(aes(fill = balance >= 0), width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.5) +
  geom_text(aes(label = signed_pounds(balance),
                y = balance + ifelse(balance < 0, -24, 24)),
            size = 3.8, colour = ink) +
  scale_fill_manual(values = c("FALSE" = col_given, "TRUE" = col_new)) +
  scale_y_continuous(labels = signed_pounds,
                     expand = expansion(mult = c(0.14, 0.08))) +
  labs(
    title = "£1,000 per adult minus the VAT cost, by income decile",
    subtitle = "Annual balance per household (£). Adult counts estimated from ONS 2024-25 sample data.\nHousehold VAT costs in 2029-30 prices; OBR CPI uplift of 14.7%.",
    x = "Household income decile (1 = poorest)", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", colour = ink, size = 16),
    plot.subtitle = element_text(colour = ink, size = 11, margin = margin(b = 12)),
    axis.text = element_text(colour = grey, size = 10.5),
    axis.title.x = element_text(colour = grey, size = 10.5, margin = margin(t = 8)),
    plot.margin = margin(16, 24, 12, 16)
  )

print(compensation)
ggsave("vat_2029_compensation.png", r, width = 10, height = 5.5, dpi = 200)


# ==========================================================================
# PART 4: Add £300 per child; cost the payments against VAT revenue
# ==========================================================================
# Same ONS table 3.1E: E19:N19 = sample persons; O17 = UK households (000s).
# Children = persons minus adults, using the survey's definitions.
# National costing applies each decile's sample ratios to one tenth of the
# 28.71m UK households. Household numbers are held at their 2024-25 level.

sample_persons <- c(1010, 990, 1130, 1080, 1210, 1200, 1180, 1210, 1100, 1050)
payment_per_child <- 300
uk_households <- 28710000

family_compensation <- compensation
family_compensation$sample_persons <- sample_persons
family_compensation$sample_children <- sample_persons - sample_adults
family_compensation$children_per_household <-
  family_compensation$sample_children / sample_households
family_compensation$child_payment <-
  family_compensation$children_per_household * payment_per_child
family_compensation$adult_only_balance <- compensation$balance
family_compensation$balance <- compensation$balance + family_compensation$child_payment

family_plot <- r %+% family_compensation +
  labs(
    title = "£1,000 per adult + £300 per child, minus the VAT cost",
    subtitle = "Annual balance per household (£). Adult and child counts estimated from ONS 2024-25 sample data.\nHousehold VAT costs in 2029-30 prices; OBR CPI uplift of 14.7%."
  )
ggsave("vat_2029_children.png", family_plot, width = 10, height = 5.5, dpi = 200)

estimated_adults <- mean(compensation$adults_per_household) * uk_households
estimated_children <- mean(family_compensation$children_per_household) * uk_households
# Fiscal envelope supplied by the user. £25bn / £500 implies 50m eligible adults.
eligible_adults <- 50000000
policy_vat_revenue_bn <- 54.7
threshold_revenue_bn <- 1.8
stamp_duty_abolition_bn <- 24.5
payment_budget_bn <- policy_vat_revenue_bn + threshold_revenue_bn - stamp_duty_abolition_bn
adult_payment_cost_bn <- eligible_adults * payment_per_adult / 1e9
child_payment_cost_bn <- estimated_children * payment_per_child / 1e9
fiscal_before_children_bn <- payment_budget_bn - adult_payment_cost_bn
fiscal_after_children_bn <- fiscal_before_children_bn - child_payment_cost_bn

# Household VAT cost is deducted once. Fiscal revenue is already in 2029-30 pounds.
fiscal <- data.frame(
  item = c("VAT reform revenue", "Lower registration threshold", "Abolish stamp duty",
           "£1,000 per adult", "Balance before child payment",
           "£300 per child", "Final fiscal balance"),
  bn = c(policy_vat_revenue_bn, threshold_revenue_bn, -stamp_duty_abolition_bn,
         -adult_payment_cost_bn, fiscal_before_children_bn,
         -child_payment_cost_bn, fiscal_after_children_bn)
)
print(family_compensation)
print(fiscal)


# ==========================================================================
# PART 5: Rebalance payments, retaining stamp-duty abolition
# ==========================================================================
# Objective: maximise the lowest average balance among deciles 1-3.
# Illustrative policy choice: retain at least £300 per adult.
# Search round £50 payment levels, subject to the national payment budget.
# Without an adult floor, this objective puts all money into child payments;
# it does not protect childless households. Decile results are averages.

adult_floor <- 300
payment_step <- 50
protected_deciles <- 1:3
retained_surplus_bn <- 7

optimise_payments <- function(budget_bn, adult_minimum = adult_floor) {
  options <- expand.grid(
    adult = seq(adult_minimum, floor(budget_bn * 1e9 / eligible_adults / payment_step) * payment_step,
                by = payment_step),
    child = seq(0, floor(budget_bn * 1e9 / estimated_children / payment_step) * payment_step,
                by = payment_step)
  )
  options$cost_bn <- (options$adult * eligible_adults + options$child * estimated_children) / 1e9
  options <- options[options$cost_bn <= budget_bn + 1e-9, ]
  options$worst_poor_balance <- vapply(seq_len(nrow(options)), function(i) {
    min(options$adult[i] * compensation$adults_per_household[protected_deciles] +
        options$child[i] * family_compensation$children_per_household[protected_deciles] -
        compensation$vat_cost_2029[protected_deciles])
  }, numeric(1))
  options[order(-options$worst_poor_balance, options$cost_bn, -options$adult), ][1, ]
}

preferred <- optimise_payments(payment_budget_bn - retained_surplus_bn)
full_budget <- optimise_payments(payment_budget_bn)

payment_options <- data.frame(
  option = c("£1,000 / £300 scenario", "£800 / £400 example", "Essay baseline",
             "Keep £7bn surplus", "Use up to £32bn"),
  adult = c(1000, 800, 500, preferred$adult, full_budget$adult),
  child = c(300, 400, 0, preferred$child, full_budget$child)
)
payment_options$cost_bn <- (payment_options$adult * eligible_adults +
                            payment_options$child * estimated_children) / 1e9
payment_options$surplus_bn <- payment_budget_bn - payment_options$cost_bn
payment_options$poorest_balance <- payment_options$adult * compensation$adults_per_household[1] +
  payment_options$child * family_compensation$children_per_household[1] - compensation$vat_cost_2029[1]

optimised_distribution <- data.frame(
  decile = compensation$decile,
  baseline = 500 * compensation$adults_per_household - compensation$vat_cost_2029,
  selected = preferred$adult * compensation$adults_per_household +
    preferred$child * family_compensation$children_per_household - compensation$vat_cost_2029,
  full_budget = full_budget$adult * compensation$adults_per_household +
    full_budget$child * family_compensation$children_per_household - compensation$vat_cost_2029
)
optimised_distribution$change <- optimised_distribution$selected - optimised_distribution$baseline

# A rigorous lower bound: spend only on the recipient type giving the poorest
# decile most payment per fiscal pound, without an adult-payment floor.
minimum_cost_poorest_bn <- compensation$vat_cost_2029[1] /
  max(compensation$adults_per_household[1] / eligible_adults,
      family_compensation$children_per_household[1] / estimated_children) / 1e9

optimisation_long <- rbind(
  data.frame(decile = optimised_distribution$decile,
             panel = "Net after the VAT cost (2029-30)", value = optimised_distribution$selected),
  data.frame(decile = optimised_distribution$decile,
             panel = "Change versus £500 per adult only", value = optimised_distribution$change)
)
optimisation_long$panel <- factor(optimisation_long$panel,
  levels = c("Net after the VAT cost (2029-30)", "Change versus £500 per adult only"))

optimisation_plot <- ggplot(optimisation_long, aes(decile, value)) +
  geom_col(aes(fill = value >= 0), width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.5) +
  geom_text(aes(label = signed_pounds(value), vjust = ifelse(value >= 0, -0.55, 1.4)),
            colour = ink, size = 3.3) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("FALSE" = col_given, "TRUE" = col_new)) +
  scale_y_continuous(labels = signed_pounds, expand = expansion(mult = c(0.22, 0.22))) +
  labs(title = sprintf("£%d per adult + £%d per child: more for the poorest three deciles",
                       preferred$adult, preferred$child),
       subtitle = "Compared with £500 per adult only. Annual amounts per household (£).\nStamp-duty abolition retained; payment cost £25.0bn; fiscal surplus £7.0bn.",
       x = "Household income decile (1 = poorest)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = bg, colour = NA),
        panel.background = element_rect(fill = bg, colour = NA),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "#E2DED2", linewidth = 0.35),
        plot.title.position = "plot", plot.title = element_text(face = "bold", size = 15, colour = ink),
        plot.subtitle = element_text(size = 11, colour = ink, margin = margin(b = 12)),
        strip.text = element_text(face = "bold", hjust = 0, colour = ink),
        axis.text = element_text(colour = grey), axis.title.x = element_text(colour = grey),
        plot.margin = margin(16, 24, 12, 16))
ggsave("vat_2029_optimised.png", optimisation_plot, width = 10, height = 7, dpi = 200)
print(payment_options)
print(optimised_distribution)
cat(sprintf("Minimum cost to make decile 1 break even (no adult floor): £%.1fbn\n", minimum_cost_poorest_bn))


# ==========================================================================
# PART 6: Household inputs by decile, in 2029-30 prices
# ==========================================================================
# OBR Economic and fiscal outlook, October 2026, table A.3 (financial-year CPI).
# https://www.gov.uk/government/publications/october-2026-economic-and-fiscal-outlook/economic-and-fiscal-outlook-october-2026
# Compound 2025-26 through 2029-30 to move the 2024-25 spending base forward.
# Household composition and eligible populations stay fixed. Payment dials are
# nominal 2029-30 pounds. Fiscal revenue inputs are already in 2029-30 pounds.
household_deciles <- data.frame(
  decile = 1:10,
  adults = compensation$adults_per_household,
  children = family_compensation$children_per_household,
  vat_cost_2024 = dist$net,
  vat_cost_2029 = dist$net * inflation_factor_2029,
  spending_2024 = spend * 52,
  sample_households = sample_households,
  sample_adults = sample_adults,
  sample_children = sample_persons - sample_adults
)


# ==========================================================================
# PART 7: One payment covering two years
# ==========================================================================
# Annual VAT cost = original cost x compounded CPI, once.
# Two-year VAT cost = annual VAT cost x 2 years, once.
# Positive net cost means worse off; negative means better off.
# Both years use 2029-30 prices, with no further year-two inflation.

calculate_two_year_scenario <- function(adult_payment = 300, child_payment = 800,
                                      inflation_rates = cpi_rates) {
  stopifnot(length(adult_payment) == 1, is.finite(adult_payment), adult_payment >= 0,
            length(child_payment) == 1, is.finite(child_payment), child_payment >= 0,
            length(inflation_rates) == 5, all(is.finite(inflation_rates)),
            all(inflation_rates > -100))
  years <- 2
  weeks <- 104
  factor <- prod(1 + inflation_rates / 100)
  households <- household_deciles
  households$vat_annual <- households$vat_cost_2024 * factor
  households$vat_two_years <- households$vat_annual * years
  households$payment_once <- households$adults * adult_payment +
    households$children * child_payment
  households$net_cost_two_years <- households$vat_two_years - households$payment_once
  households$net_cost_weekly <- households$net_cost_two_years / weeks
  payment_cost_two_years_bn <- (eligible_adults * adult_payment +
                               estimated_children * child_payment) / 1e9
  payment_cost_annual_bn <- payment_cost_two_years_bn / years
  list(households = households, inflation_factor = factor,
       payment_cost_two_years_bn = payment_cost_two_years_bn,
       payment_cost_annual_bn = payment_cost_annual_bn,
       fiscal_balance_annual_bn = payment_budget_bn - payment_cost_annual_bn)
}


# ==========================================================================
# PART 8: Five-year revenue timeline — payment split over 2029-30 and 2030-31
# ==========================================================================
# VAT reform: five-year revenue timeline (£bn, static).
# IFS: https://ifs.org.uk/publications/options-tax-increases
# Resolution Foundation: https://www.resolutionfoundation.org/app/uploads/2025/09/Call-of-duties.pdf
# HMRC: https://www.gov.uk/government/statistics/direct-effects-of-illustrative-tax-changes
vat_timeline <- local({
  zero_reduced_cost <- 83.1
  energy_cost <- 7.8
  one_point_vat <- 9.9
  stamp_duty <- 24.5
  threshold_at_20 <- 2.0
  new_rate <- 18
  payment <- 900
  citizens_m <- 62.5 # Own estimate, as supplied for this timeline.
  vat_growth <- 0.038
  stamp_growth <- 0.06

  zero_rated_gain <- (zero_reduced_cost - energy_cost) * new_rate / 20
  energy_gain <- energy_cost * (new_rate - 5) / 15
  standard_loss <- one_point_vat * (20 - new_rate)
  net_vat <- zero_rated_gain + energy_gain - standard_loss
  threshold <- threshold_at_20 * new_rate / 20
  payment_cost <- payment * citizens_m / 1000
  years <- c("2029-30", "2030-31", "2031-32", "2032-33", "2033-34")
  t <- 0:4
  timeline <- data.frame(
    year=years,
    net_vat=net_vat*(1+vat_growth)^t,
    threshold=threshold*(1+vat_growth)^t,
    stamp_duty=-stamp_duty*(1+stamp_growth)^t
  )
  timeline$left_over <- timeline$net_vat + timeline$threshold + timeline$stamp_duty
  timeline$payment <- c(rep(-payment_cost / 2, 2), rep(0, length(t) - 2))
  timeline$net <- timeline$left_over + timeline$payment
  timeline$running <- cumsum(timeline$net)
  timeline$shown_savings <- timeline$net
  timeline$chart_total <- -timeline$stamp_duty - timeline$payment + timeline$shown_savings
  items <- c('Stamp-duty cut','Half one-off payment','Savings')
  long <- data.frame(
    year=factor(rep(timeline$year,3),levels=years),
    item=factor(rep(items,each=nrow(timeline)),levels=items),
    value=c(-timeline$stamp_duty,-timeline$payment,timeline$shown_savings)
  )
  timeline_plot <- ggplot(long,aes(year,value,fill=item)) +
    geom_col(width=.6,colour='#FAF5EC',linewidth=.6,position=position_stack(reverse=TRUE)) +
    geom_hline(yintercept=0,colour='#333333',linewidth=.4) +
    geom_text(data=timeline,aes(x=year,y=chart_total,label=sprintf('£%.1fbn',chart_total)),inherit.aes=FALSE,vjust=-.6,size=3.6,colour='#333333') +
    geom_text(aes(label=ifelse(value>0 & !(item=='Savings' & year %in% years[1:2]),sprintf('%.1f',value),'')),position=position_stack(vjust=.5,reverse=TRUE),colour='white',size=3.6) +
    scale_fill_manual(values=c('Stamp-duty cut'='#777777','Half one-off payment'='#A32040','Savings'='#1F4D3A')) +
    scale_y_continuous(expand=expansion(mult=c(0,.12))) +
    labs(
      title='The payment is paid off within two years; more than £33bn a year is free after that',
      subtitle='Distribution of savings from a lower, uniform VAT rate (£bn per year).',
      x=NULL,y=NULL,fill=NULL
    ) +
    theme_minimal(base_size=12) +
    theme(plot.background=element_rect(fill='#FAF5EC',colour=NA),panel.background=element_rect(fill='#FAF5EC',colour=NA),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),panel.grid.major.y=element_line(colour='#e6e6e6',linewidth=.3),legend.position='top',legend.justification='left',plot.title=element_text(face='bold',size=15),plot.title.position='plot',plot.subtitle=element_text(size=11,margin=margin(b=8)),axis.text=element_text(colour='#656565',size=10.5),plot.caption=element_text(size=9,colour='#656565'),plot.margin=margin(10,24,8,16))
  stopifnot(all(long$value>=0),isTRUE(all.equal(
    as.numeric(tapply(long$value,long$year,sum)),timeline$chart_total)))
  list(data=timeline,plot=timeline_plot)
})



# ==========================================================================
# PART 9: Companion page (vat_companion.html): every figure, table and chart
# ==========================================================================
# Scenario shown on the page: £900 per adult + £900 per child, paid once to
# cover 2029-30 and 2030-31. Fiscal inputs (£54.7bn, £1.8bn, £24.5bn) are
# set in Part 4; household VAT costs come from Parts 6-7; the five-year
# timeline from Part 8. Running this script writes the four charts and
# companion_data.json, which mirrors the <script id="companion-data"> block.

companion_adult <- 900
companion_child <- 900
companion <- calculate_two_year_scenario(companion_adult, companion_child)
hh <- companion$households
decile_levels <- c(1:10, "Median")

# 9a. Fiscal summary table (£bn, 2029-30) at the top of the page
companion_fiscal <- data.frame(
  item = c("VAT reform revenue", "Lower VAT threshold", "Abolish stamp duty",
           "Half the one-off payment cost", "Surplus"),
  bn = c(policy_vat_revenue_bn, threshold_revenue_bn, -stamp_duty_abolition_bn,
         -companion$payment_cost_annual_bn, companion$fiscal_balance_annual_bn)
)
print(companion_fiscal)
cat(sprintf("Population covered: %.0fm adults + %.2fm children = %.2fm people\n",
            eligible_adults / 1e6, estimated_children / 1e6,
            (eligible_adults + estimated_children) / 1e6))
cat(sprintf("Payments: £%.2fbn over two years, £%.2fbn per year\n",
            companion$payment_cost_two_years_bn, companion$payment_cost_annual_bn))

# 9b. Revenue arithmetic shown under "Show IFS figures"
revenue_arithmetic <- data.frame(
  item = c("New revenue from broadening the base at 18%",
           "Revenue per 1 percentage point of the standard rate",
           "Cost of cutting the rate from 20% to 18%", "Net revenue"),
  bn = c(new_revenue, yield_1pp, -given_up, net_raised)
)
print(revenue_arithmetic)

# 9c. Household table by decile, with the median of each column
companion_table <- data.frame(
  decile = as.character(1:10),
  adults = hh$adults, children = hh$children,
  vatAnnual = hh$vat_annual, vatTwoYears = hh$vat_two_years,
  payment = hh$payment_once,
  netCostTwoYears = hh$net_cost_two_years, netCostWeekly = hh$net_cost_weekly
)
companion_median <- cbind(decile = "Median",
                          as.data.frame(lapply(companion_table[-1], median)))
print(rbind(companion_table, companion_median), digits = 6)

family_payment <- 2 * companion_adult + 2 * companion_child
cat(sprintf("Family example (2 adults, 2 children): £%.2f over two years (£%.2f/week)\n",
            family_payment, family_payment / 104))
cat(sprintf("2029-30 inflation uplift: +%.1f%%\n", 100 * (companion$inflation_factor - 1)))

# 9d. Chart: annual VAT cost by decile in 2029-30 prices, with a median bar
companion_long <- long
companion_long$value  <- long$value * companion$inflation_factor
companion_long$decile <- factor(as.character(long$decile), levels = decile_levels)
median_net <- median(hh$vat_annual)
median_pct <- median(dist$pct_spend)
companion_long <- rbind(
  companion_long,
  data.frame(decile = factor("Median", levels = decile_levels),
             component = "Median (net)", value = median_net)
)
companion_long$component <- factor(companion_long$component,
                                   levels = c(levels(long$component), "Median (net)"))
companion_points <- data.frame(
  decile = factor(decile_levels, levels = decile_levels),
  net    = c(hh$vat_annual, median_net),
  pct    = c(dist$pct_spend, median_pct),
  label_y = c(dist$gross * companion$inflation_factor, median_net) + 40
)

companion_deciles_plot <- ggplot() +
  geom_hline(yintercept = seq(-500, 2000, 500), colour = "#E2DED2", linewidth = 0.35) +
  geom_col(data = companion_long, aes(x = decile, y = value, fill = component), width = 0.7) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.5) +
  geom_point(data = companion_points, aes(x = decile, y = net), size = 3, shape = 21,
             fill = ink, colour = "white", stroke = 0.8) +
  geom_text(data = companion_points,
            aes(x = decile, y = label_y,
                label = sprintf("£%s\n%.1f%%", formatC(round(net), format = "d", big.mark = ","), pct)),
            vjust = 0, size = 3.1, colour = ink, lineheight = 0.9) +
  scale_fill_manual(values = c("Food" = col_new, "Energy" = "#4F7F6B",
                               "Other zero-rated" = "#9DBFAF",
                               "Cheaper standard-rated goods" = col_given,
                               "Median (net)" = "#9B9B9B"), name = NULL) +
  scale_y_continuous(breaks = seq(-500, 2000, 500), limits = c(-800, 2250),
                     labels = function(v) ifelse(v == 2000, "£2,000", format(v, big.mark = ","))) +
  labs(
    title = sprintf(
      "The poorest households would pay %.1f%% more of their spending, the richest %.1f%%",
      dist$pct_spend[1], dist$pct_spend[10]),
    subtitle = "Annual VAT cost by income decile (£, 2029–30 prices).",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", colour = ink, size = 15),
    plot.subtitle = element_text(colour = ink, size = 11, margin = margin(b = 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 6, -40),
    axis.text = element_text(colour = grey, size = 10.5),
    plot.margin = margin(16, 24, 12, 16)
  )
ggsave("vat_2029_companion_deciles.png", companion_deciles_plot, width = 10, height = 4.5, dpi = 200)

# 9e. Chart: net cost per household per week, with the median
signed_weekly <- function(x) {
  paste0(ifelse(x < 0, "−", "+"), "£", formatC(abs(x), format = "f", digits = 2))
}
weekly <- data.frame(
  decile = factor(decile_levels, levels = decile_levels),
  value  = c(hh$net_cost_weekly, median(hh$net_cost_weekly)),
  kind   = c(rep("decile", 10), "median")
)
companion_weekly_plot <- ggplot(weekly, aes(x = decile, y = value)) +
  geom_hline(yintercept = pretty(c(0, weekly$value)), colour = "#E2DED2", linewidth = 0.35) +
  geom_col(aes(fill = kind), width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, colour = ink, linewidth = 0.5) +
  geom_text(aes(label = signed_weekly(value), vjust = ifelse(value >= 0, -0.6, 1.5)),
            size = 3.8, colour = ink) +
  scale_fill_manual(values = c("decile" = "#333333", "median" = "#9B9B9B")) +
  scale_y_continuous(labels = function(v) paste0(ifelse(v < 0, "−", ""), "£", abs(v)),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Net cost per household per week",
    subtitle = "Two years’ VAT cost minus the one-off payment, divided by 104. Positive = worse off; negative = better off.",
    x = "Household income decile (1 = poorest)", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", colour = ink, size = 15),
    plot.subtitle = element_text(colour = ink, size = 11, margin = margin(b = 8)),
    axis.text = element_text(colour = grey, size = 10.5),
    axis.title.x = element_text(colour = grey, size = 10.5, margin = margin(t = 8)),
    plot.margin = margin(16, 24, 12, 16)
  )
ggsave("vat_2029_companion_weekly.png", companion_weekly_plot, width = 10, height = 3.6, dpi = 200)

# 9f. Five-year timeline chart and its supporting figures
print(vat_timeline$data)
ggsave("vat_2029_timeline.png", vat_timeline$plot, width = 10, height = 4.5, dpi = 200)

# HMRC one-point VAT yield, £bn: the basis for the 3.8% VAT growth assumption.
# 2026-27 to 2028-29 from HMRC "Direct effects of illustrative tax changes"
# (June 2025); 2029-30 from the IFS.
hmrc_1pp <- c("2026-27" = 8.8, "2027-28" = 9.2, "2028-29" = 9.55, "2029-30" = 9.9)
cat("Year-on-year growth in the one-point yield (%):\n")
print(round(100 * diff(hmrc_1pp) / head(hmrc_1pp, -1), 1))

# Sensitivity of the 2033-34 amount left over to stamp-duty growth.
last <- nrow(vat_timeline$data)
stamp_sensitivity <- sapply(c(0.04, 0.06, 0.08), function(g) {
  vat_timeline$data$net_vat[last] + vat_timeline$data$threshold[last] -
    stamp_duty_abolition_bn * (1 + g)^(last - 1)
})
cat(sprintf("Left over in 2033-34: £%.1fbn at 4%%, £%.1fbn at 6%%, £%.1fbn at 8%% stamp-duty growth\n",
            stamp_sensitivity[1], stamp_sensitivity[2], stamp_sensitivity[3]))

# 9g. Data block embedded in the companion page
if (requireNamespace("jsonlite", quietly = TRUE)) {
  companion_json <- list(
    inflation = companion$inflation_factor,
    adultCostTwoYears = eligible_adults * companion_adult / 1e9,
    childCostTwoYears = estimated_children * companion_child / 1e9,
    costTwoYears = companion$payment_cost_two_years_bn,
    costAnnual = companion$payment_cost_annual_bn,
    budgetAnnual = payment_budget_bn,
    balanceAnnual = companion$fiscal_balance_annual_bn,
    rows = companion_table,
    median = companion_median,
    state = list(adult = companion_adult, child = companion_child, rates = cpi_rates),
    DATA = list(deciles = household_deciles, cpiYears = cpi_years, cpiRates = cpi_rates,
                adultPopulation = eligible_adults, childPopulation = estimated_children,
                vatRevenue = policy_vat_revenue_bn, thresholdRevenue = threshold_revenue_bn,
                stampCost = stamp_duty_abolition_bn)
  )
  jsonlite::write_json(companion_json, "companion_data.json",
                       auto_unbox = TRUE, digits = NA, pretty = TRUE)
}
