tests
testRegisterCustomTranslator

	NaturalLanguageTranslator current: RegisterableTranslatorMock new.
	"Our custom translator translates the string differently"
	self assert: 'TestCase' translated = 'Testfall'.
	
	