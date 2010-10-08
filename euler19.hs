let f = \year month day -> let t = fromGregorianValid year month day in case t of {Nothing -> Nothing ; Just s ->  Just $ Data.Time.Format.formatTime System.Locale.defaultTimeLocale "%a" s} in length $ filter ( == "Sun") $ Data.Maybe.catMaybes $ map (\(y, m, d) -> f y m d) $ [(y,m,d) | y <- [1901..2000], m <- [1..12], d <- [1]]
