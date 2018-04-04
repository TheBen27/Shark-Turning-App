import Test.Hspec
import qualified WindowerSpec (spec)
import qualified CsvImporterSpec (spec)

main = hspec spec

spec :: Spec
spec = do
    describe "Windower" WindowerSpec.spec
    describe "CsvImporter" CsvImporterSpec.spec
