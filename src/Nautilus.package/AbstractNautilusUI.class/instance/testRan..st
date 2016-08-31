system announcements
testRan: anAnnouncement
	| testCaseClass classTested |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	"testResult actually returns the TestCase class ran"
	testCaseClass := anAnnouncement testResult.
	classTested := testCaseClass correspondingClassTested.
	(self classWidget getClassesList
		includesAnyOf:
			{testCaseClass.
			classTested.
			classTested class})
		ifTrue: [ self
				removeClassFromMethodsIconsCache: testCaseClass;
				removeClassFromMethodsIconsCache: classTested;
				removeClassFromMethodsIconsCache: classTested class;
				updateClassView;
				updateCategoryAndMethodList ]