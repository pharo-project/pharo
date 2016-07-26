testing
isAnotherValueStored
	"Return true if the another setting value is stored."
	| storedValue |
	storedValue := self storedValue.
	^ storedValue notNil and: [ storedValue ~= self settingValue ]