module SimpleTime where

---- Time ----

-- Usually, time is represented as a single number: time since some
-- agreed-upon fixed date. However, we don't need to do any computations
-- like "two hours after now" in this lab, so the below representation
-- is sufficient and much easier to display.
data Time = Time
    { year   :: Int
    , month  :: Int
    , day    :: Int
    , hour   :: Int
    , minute :: Int
    , second :: Int
    } deriving (Eq, Ord, Bounded)

-- Example: showWithAtLeastNDigits 3 8  => "008"
showWithAtLeastNDigits :: Int -> Int -> String
showWithAtLeastNDigits digits int =
    replicate (digits - length intStr) '0' ++ intStr
    where intStr = show int

-- "2017-10-07T14:09:53" (like ISO 8601)
instance Show Time where
    show time =
        showWithAtLeastNDigits 4 (year time)   ++ "-" ++
        showWithAtLeastNDigits 2 (month time)  ++ "-" ++
        showWithAtLeastNDigits 2 (day time)    ++ "T" ++
        showWithAtLeastNDigits 2 (hour time)   ++ ":" ++
        showWithAtLeastNDigits 2 (minute time) ++ ":" ++
        showWithAtLeastNDigits 2 (second time)

instance Read Time where
  readsPrec _ timeStr =
    case timeStr of
      y4:y3:y2:y1:'-':mon2:mon1:'-':d2:d1:'T':h2:h1:':':min2:min1:':':s2:s1:rest ->
        [(Time
            { year   = read [y4, y3, y2, y1]
            , month  = read [mon2, mon1]
            , day    = read [d2, d1]
            , hour   = read [h2, h1]
            , minute = read [min2, min1]
            , second = read [s2, s1]
            } , rest)]
      _ -> error $ "Bad time string: " ++ timeStr

