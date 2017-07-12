Ensemble <- setRefClass("Ensemble",
  fields = c(
    mod = "ANY",
    node_map = "list"),
  contains="VIRTUAL",
  methods = list(
    predict_sparse_nodes = function(newdata, n.trees) {"Must Implement"},
    make_node_map = function() {"Must Implement"},
    get_tree_position = function(i, m) {"Must Implement"},
    get_lineage = function(tid, cid) {"Must Implement"})
)