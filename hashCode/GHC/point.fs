module GHC.Point

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------

let setPoids points videos = 
    for p in points do 
        for r in p.reqs do
            r.score <- videos[r.video].size
    

//-------------------------------------------------------------------------------------------------

let computePoints (videos, points, cacheNum, cacheSize) = 
   // mettre les poids dans les requetes
   setPoids points videos
   // fusionner les requetes par video
   // calculer un score pour chaque requette