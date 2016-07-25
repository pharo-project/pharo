accessing
toValue

	^ toValue ifNil: [ toValue := toLogic brickValue: self animatedObject ]