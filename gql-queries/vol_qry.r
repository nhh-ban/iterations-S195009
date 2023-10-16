vol_qry <- function(id, from, to) {
  query <- sprintf('
  {
    trafficData(trafficRegistrationPointId: "%s") {
      volume {
        byHour(from: "%s", to: "%s") {
          edges {
            node {
              from
              to
              total {
                volumeNumbers {
                  volume
                }
              }
            }
          }
        }
      }
    }
  }', id, from, to)
  return(query)
}


GQL(
  vol_qry(
    id=stations_metadata$id[1], 
    from=to_iso8601(stations_metadata$latestData[1],-4),
    to=to_iso8601(stations_metadata$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)
