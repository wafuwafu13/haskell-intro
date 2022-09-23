{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Logic where

import InferenceRule

correctAnswer :: ( Chinpei `NotEat` Soba
                 , Kenta `NotEat` Curry
                 , Kenta `NotEat` Soba
                 )
              -> ( Tonkichi `Eat` Soba
                 , Chinpei `Eat` Curry
                 , Kenta `Eat` Ramen
                 )

correctAnswer (chinpeiUnsatisfySoba, kantaHateCurry, kantaHateSoba) =
  (tonkichiEatSoba, chinpeiEatCurry, kantaEatRamen) where
  kantaEatRamen = EatRemainFood kantaHateCurry kantaHateSoba
  chinpeiNotEatRamen = AnotherPeopleNotEat kantaEatRamen
  chinpeiEatCurry = EatRemainFood chinpeiUnsatisfySoba chinpeiNotEatRamen
  tonkichiNotEatCurry = AnotherPeopleNotEat chinpeiEatCurry
  tonkichiNotEatRamen = AnotherPeopleNotEat kantaEatRamen
  tonkichiEatSoba = EatRemainFood tonkichiNotEatCurry tonkichiNotEatRamen
