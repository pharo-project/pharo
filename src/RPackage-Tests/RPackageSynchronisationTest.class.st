Class {
	#name : 'RPackageSynchronisationTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'running' }
RPackageSynchronisationTest >> tearDown [

	SystemAnnouncer uniqueInstance unsubscribe: self.
	super tearDown
]

{ #category : 'to move to a simple Package test case' }
RPackageSynchronisationTest >> testNotRepackagedAnnouncementWhenModifyMethodByMovingInSameExtensionCategory [

	| ann class |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	class := self newClassNamed: 'NewClass' in: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	self assert: ann isNil.

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil
]

{ #category : 'to move to a simple Package test case' }
RPackageSynchronisationTest >> testNotRepackagedAnnouncementWhenMovingClassicCategories [

	| ann class |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	class := self newClassNamed: 'NewClass' in: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic'.

	self assert: ann isNil.

	class classify: #stubMethod under: 'another classic one'.

	self assert: ann isNil
]

{ #category : 'tests - recategorizing class' }
RPackageSynchronisationTest >> testRecategorizeClassRaisesClassRepackagedEvent [
	"test that when we recategorize a class, the organizer is updated so that the class name point the the new Package"

	| xPackage yPackage class ann |
	SystemAnnouncer uniqueInstance when: ClassRepackaged do: [ :a | ann := a ] for: self.

	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	class package: yPackage.

	self assert: ann isNotNil.
	self assert: ann classRepackaged equals: class.
	self assert: ann oldPackage equals: xPackage.
	self assert: ann newPackage equals: yPackage
]

{ #category : 'to move to a simple Package test case' }
RPackageSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromClassicCategoryToExtensionCategory [

	| ann class xPackage yPackage |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class classify: #stubMethod under: '*yyyyy'.

	self assert: ann isNotNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: xPackage.
	self assert: ann newPackage equals: yPackage.
	ann := nil.

	class classify: #stubMethod under: '*yyyyy-suncategory'.

	self assert: ann isNil
]

{ #category : 'to move to a simple Package test case' }
RPackageSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromDifferentExtensionCategories [

	| ann class yPackage zPackage |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	class := self newClassNamed: 'NewClass' in: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class classify: #stubMethod under: '*zzzzz'.

	self assert: ann isNotNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: yPackage.
	self assert: ann newPackage equals: zPackage
]

{ #category : 'to move to a simple Package test case' }
RPackageSynchronisationTest >> testRepackagedAnnouncementWhenModifyMethodByMovingFromExtensionCategoryToClassicCategory [

	| ann class xPackage yPackage |
	SystemAnnouncer uniqueInstance when: MethodRepackaged do: [ :a | ann := a ] for: self.

	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class classify: #stubMethod under: 'classic one'.

	self assert: ann isNotNil.
	self assert: ann methodRepackaged selector equals: #stubMethod.
	self assert: ann oldPackage equals: yPackage.
	self assert: ann newPackage equals: xPackage
]
