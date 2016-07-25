tests
testClass

	self argumentNameForInstanceOf: Class shouldBe: 'aClass'.
	self argumentNameFor: Integer shouldBe: 'aClass'.
	self argumentNameFor: Collection shouldBe: 'aClass'.