[<AutoOpen>]
module Helpers

[<RequireQualifiedAccess>]
module Map =
    /// A naive union function for maps.
    /// Whenever a key exists in both maps, the first map's
    /// entry will be added to the result map.
    let union (map1 : Map<'Key, 'T>) (map2 : Map<'Key, 'T>) : Map<'Key, 'T> =
        match map1.Count, map2.Count with
        // Optimize for empty inputs
        | 0, 0 ->
            Map.empty
        | 0, _ ->
            map2
        | _, 0 ->
            map1
        | _, _ ->
            // Start with the second map.
            // Fold over the first map, adding it's entries to the second
            // and overwriting any existing entries.
            (map2, map1)
            ||> Map.fold (fun combinedMap key value ->
                Map.add key value combinedMap)
        
