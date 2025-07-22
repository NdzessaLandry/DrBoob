
Bd<-function(){
  Sys.setenv("NEO4J_URL" = "BOLT://localhost:7687")
  Sys.setenv("NEO4J_USER" = "neo4j")
  Sys.setenv("NEO4J_PASSWORD" = "landry2003")
}

label1 <- "Person"
label2 <- "Company"
relation_type <- "WORKS_AT"

query <- sprintf("
MERGE (p:%s {name: $name1})
MERGE (c:%s {name: $name2})
MERGE (p)-[:%s]->(c)
", label1, label2, relation_type)

params <- dict(
  name1 = "Alice",
  name2 = "Neo4j Inc."
)