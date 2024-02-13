library(dplyr)
library(plotly)

# Store it in a data frame
df <- data.frame(
  source = c("a", "a", "a", "b", "b", "c", "d", "e", "e", "f", "f"),
  source_group = c("A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
  receiver = c("b", "c", "b", "b", "d", "d", "e", "a", "f", "g", "h"),
  receiver_group = c("A", "A", "A", "A", "B", "B", "B", "A", "B", "C", "C"),
  amount = c(500, 100, 50, 100, 250, 75, 3, 5, 10, 6, 3)
)

group_colors  = data.frame(
  group = c("A","B","C"),
  color = c("#6b2496", "#ff7c5c", "#00a0e6")
)


nodes = {
  t1 = df[, c("source", "source_group")]
  t2 = df[, c("receiver", "receiver_group")]
  colnames(t1) = c("name","group")
  colnames(t2) = c("name","group")
  unique(rbind(t1, t2))
}

nodes$id <- as.numeric(factor(nodes$name)) - 1

nodes <- merge(nodes, group_colors, by = 'group', sort = FALSE)

# Map source and receiver to numeric identifiers
df <- df %>%
  left_join(nodes, by = c("source" = "name")) %>%
  rename(source_id = id) %>%
  left_join(nodes, by = c("receiver" = "name")) %>%
  rename(receiver_id = id)

# In large diagrams, conflicting rownames and IDs can cause collisions
# ensure rownames are not included 
rownames(nodes) <- NULL

# Create a Sankey diagram

plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  type = "sankey",
  orientation = "h",
  node = list(
    label = nodes$name,
    color = nodes$color,
    pad = 15,
    thickness = 20,
    hovertemplate = paste("Group: %{customdata}<extra></extra>"),
    customdata = nodes$group
  ),
  link = list(
    source = df$source_id,
    target = df$receiver_id,
    value = df$amount,
    hovertemplate = paste("Amount: %{value}<extra></extra>")
  )
) %>%
  layout(title = "Sankey Diagram with Group Colors")
