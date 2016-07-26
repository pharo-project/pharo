*GT-SpotterExtensions-Core-private
withClassesFixedForNautilus
	classSelectors ifEmpty: [ 
		metaClassSelectors keysAndValuesDo: [ :class :selectors | 
			classSelectors at: class ifAbsentPut: [ IdentitySet new ] ] ]