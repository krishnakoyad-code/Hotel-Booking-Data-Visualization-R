hotel_data <- read.csv("hotel_bookings_cleaned.csv")

dim(hotel_data)
head(hotel_data)
str(hotel_data)
install.packages("ggplot2")
library(ggplot2)
ggplot(hotel_data, aes(x = hotel, fill = hotel)) +
  geom_bar() +
  labs(
    title = "Number of Bookings by Hotel Type",
    x = "Hotel Type",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
hotel_data$cancellation_status <- ifelse(
  hotel_data$is_canceled == 1,
  "Cancelled",
  "Not Cancelled"
)

ggplot(hotel_data, aes(x = cancellation_status, fill = cancellation_status)) +
  geom_bar() +
  labs(
    title = "Hotel Booking Cancellation Status",
    x = "Booking Status",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
hotel_data$arrival_date_month <- factor(
  hotel_data$arrival_date_month,
  levels = c(
    "January", "February", "March", "April",
    "May", "June", "July", "August",
    "September", "October", "November", "December"
  ),
  ordered = TRUE
)

monthly_bookings <- as.data.frame(
  table(hotel_data$arrival_date_month)
)

colnames(monthly_bookings) <- c("Month", "Bookings")

ggplot(
  monthly_bookings,
  aes(x = Month, y = Bookings, group = 1)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Monthly Trend of Hotel Bookings",
    x = "Arrival Month",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(
    bins = 30,
    color = "white"
  ) +
  labs(
    title = "Distribution of Booking Lead Time",
    x = "Lead Time (Days)",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggplot(
  hotel_data,
  aes(x = lead_time, y = adr)
) +
  geom_point(
    alpha = 0.25,
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Lead Time and ADR",
    x = "Lead Time (Days)",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggplot(
  hotel_data,
  aes(x = hotel, y = adr, fill = hotel)
) +
  geom_boxplot() +
  labs(
    title = "Average Daily Rate by Hotel Type",
    x = "Hotel Type",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggplot(
  hotel_data,
  aes(x = reorder(market_segment, market_segment,
                  FUN = function(x) -length(x)),
      fill = market_segment)
) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Hotel Bookings by Market Segment",
    x = "Market Segment",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
cancellation_by_hotel <- aggregate(
  is_canceled ~ hotel,
  data = hotel_data,
  FUN = function(x) mean(x) * 100
)

ggplot(
  cancellation_by_hotel,
  aes(x = hotel, y = is_canceled, fill = hotel)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(is_canceled, 1), "%")),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Cancellation Rate by Hotel Type",
    x = "Hotel Type",
    y = "Cancellation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggplot(
  hotel_data,
  aes(x = hotel, fill = cancellation_status)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = "Booking Status Distribution by Hotel Type",
    x = "Hotel Type",
    y = "Percentage of Bookings",
    fill = "Booking Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
monthly_hotel <- as.data.frame(
  table(
    hotel_data$arrival_date_month,
    hotel_data$hotel
  )
)

colnames(monthly_hotel) <- c(
  "Month",
  "Hotel",
  "Bookings"
)

ggplot(
  monthly_hotel,
  aes(
    x = Month,
    y = Bookings,
    group = Hotel,
    color = Hotel
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Monthly Booking Trend by Hotel Type",
    x = "Arrival Month",
    y = "Number of Bookings",
    color = "Hotel Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )
ggsave(
  "01_bookings_by_hotel.png",
  width = 8,
  height = 5,
  dpi = 300
)
hotel_data$cancellation_status <- ifelse(
  hotel_data$is_canceled == 1,
  "Cancelled",
  "Not Cancelled"
)

ggplot(hotel_data, aes(x = cancellation_status, fill = cancellation_status)) +
  geom_bar() +
  labs(
    title = "Hotel Booking Cancellation Status",
    x = "Booking Status",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "02_cancellation_status.png",
  width = 8,
  height = 5,
  dpi = 300
)
# ============================================================
# SAVE ALL REMAINING WEEK 2 VISUALIZATIONS
# Visualization 3 to Visualization 10
# ============================================================


# ------------------------------------------------------------
# VISUALIZATION 3: Monthly Booking Trend
# ------------------------------------------------------------

hotel_data$arrival_date_month <- factor(
  hotel_data$arrival_date_month,
  levels = c(
    "January", "February", "March", "April",
    "May", "June", "July", "August",
    "September", "October", "November", "December"
  ),
  ordered = TRUE
)

monthly_bookings <- as.data.frame(
  table(hotel_data$arrival_date_month)
)

colnames(monthly_bookings) <- c("Month", "Bookings")

p3 <- ggplot(
  monthly_bookings,
  aes(x = Month, y = Bookings, group = 1)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Monthly Trend of Hotel Bookings",
    x = "Arrival Month",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "03_monthly_booking_trend.png",
  plot = p3,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# VISUALIZATION 4: Lead Time Distribution
# ------------------------------------------------------------

p4 <- ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(
    bins = 30,
    color = "white"
  ) +
  labs(
    title = "Distribution of Booking Lead Time",
    x = "Lead Time (Days)",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "04_lead_time_distribution.png",
  plot = p4,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# VISUALIZATION 5: Lead Time vs ADR
# ------------------------------------------------------------

p5 <- ggplot(
  hotel_data,
  aes(x = lead_time, y = adr)
) +
  geom_point(
    alpha = 0.25,
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Lead Time and ADR",
    x = "Lead Time (Days)",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "05_lead_time_vs_adr.png",
  plot = p5,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# VISUALIZATION 6: ADR by Hotel Type
# ------------------------------------------------------------

p6 <- ggplot(
  hotel_data,
  aes(x = hotel, y = adr, fill = hotel)
) +
  geom_boxplot() +
  labs(
    title = "Average Daily Rate by Hotel Type",
    x = "Hotel Type",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "06_adr_by_hotel_type.png",
  plot = p6,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# VISUALIZATION 7: Bookings by Market Segment
# ------------------------------------------------------------

p7 <- ggplot(
  hotel_data,
  aes(
    x = reorder(
      market_segment,
      market_segment,
      FUN = function(x) -length(x)
    ),
    fill = market_segment
  )
) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Hotel Bookings by Market Segment",
    x = "Market Segment",
    y = "Number of Bookings"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "07_bookings_by_market_segment.png",
  plot = p7,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# VISUALIZATION 8: Cancellation Rate by Hotel Type
# ------------------------------------------------------------

cancellation_by_hotel <- aggregate(
  is_canceled ~ hotel,
  data = hotel_data,
  FUN = function(x) mean(x) * 100
)

p8 <- ggplot(
  cancellation_by_hotel,
  aes(x = hotel, y = is_canceled, fill = hotel)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(is_canceled, 1), "%")),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Cancellation Rate by Hotel Type",
    x = "Hotel Type",
    y = "Cancellation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "08_cancellation_rate_by_hotel.png",
  plot = p8,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# VISUALIZATION 9: Booking Status Distribution by Hotel Type
# ------------------------------------------------------------

hotel_data$cancellation_status <- ifelse(
  hotel_data$is_canceled == 1,
  "Cancelled",
  "Not Cancelled"
)

p9 <- ggplot(
  hotel_data,
  aes(x = hotel, fill = cancellation_status)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = "Booking Status Distribution by Hotel Type",
    x = "Hotel Type",
    y = "Percentage of Bookings",
    fill = "Booking Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  "09_booking_status_by_hotel.png",
  plot = p9,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# VISUALIZATION 10: Monthly Booking Trend by Hotel Type
# ------------------------------------------------------------

monthly_hotel <- as.data.frame(
  table(
    hotel_data$arrival_date_month,
    hotel_data$hotel
  )
)

colnames(monthly_hotel) <- c(
  "Month",
  "Hotel",
  "Bookings"
)

p10 <- ggplot(
  monthly_hotel,
  aes(
    x = Month,
    y = Bookings,
    group = Hotel,
    color = Hotel
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Monthly Booking Trend by Hotel Type",
    x = "Arrival Month",
    y = "Number of Bookings",
    color = "Hotel Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

ggsave(
  "10_monthly_trend_by_hotel.png",
  plot = p10,
  width = 8,
  height = 5,
  dpi = 300
)

print("All Week 2 visualizations saved successfully!")