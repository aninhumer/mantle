
import Mantle.Logic
import Mantle.Synchronous

counter :: Synchronous (Logic Int)
counter = do
    val <- reg 0
    val <=: (rd val + 1)
    return (rd val)

