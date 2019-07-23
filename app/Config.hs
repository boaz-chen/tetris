module Config where


height = 20 :: Int
width = 10 :: Int
blockSize = 30 :: Float
boardOffsetX = (blockSize / 2) - blockSize * (fromIntegral width :: Float) / 2
boardOffsetY = (blockSize / 2) - blockSize * (fromIntegral height :: Float) / 2
framesPerSecond = 5 :: Int
