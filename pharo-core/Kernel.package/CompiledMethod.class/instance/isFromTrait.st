testing
isFromTrait
	"Return true for methods that have been included from Traits"
	^ self origin isTrait and: [ self origin ~= self methodClass ]