Class {
	#name : #PackageTreeNautilusTest,
	#superclass : #TestCase,
	#instVars : [
		'nautilus',
		'lastValue',
		'compiledMethod',
		'expectedMethodCategory',
		'expectedCompiledMethod'
	],
	#category : #Nautilus-Tests
}

{ #category : #asserting }
PackageTreeNautilusTest >> assertMethodWidget [
	"Assert changes for new fast table"
	AbstractWidget listClass = FTPluggableIconListMorphAdaptor 
		ifTrue: [ ^ self assertMethodWidgetAdaptor ].
	^ self assertMethodWidgetPluggableListMorph
]

{ #category : #asserting }
PackageTreeNautilusTest >> assertMethodWidgetAdaptor [
	| index item |
	"test result changed because nautilus behavior has changed.
	issue 14890 (nhe)"
	index := nautilus ui methodWidget methodsList selectedRowIndex.
	nautilus selectedMethod ifNotNil: [ 
		self assert: index notNil.
		self deny: index = 0.
		item := nautilus ui methodWidget methodsList dataSource elementAt: index.
		self assert: item equals: compiledMethod ]
]

{ #category : #asserting }
PackageTreeNautilusTest >> assertMethodWidgetPluggableListMorph [
	| index item |
	"test result changed because nautilus behavior has changed.
	issue 14890 (nhe)"
	self assert: nautilus ui methodWidget methodsList listMorph submorphs notEmpty.
	index := nautilus ui methodWidget methodsList listMorph selectedRow.
	nautilus selectedMethod ifNotNil: [ 
		self assert: index notNil.
		item := nautilus ui methodWidget methodsList listMorph item: index.
		self assert: item originalObject equals: compiledMethod ]
]

{ #category : #asserting }
PackageTreeNautilusTest >> assertPackageMorph: eachMorph [
	| tagMorph tag |
	self assert: eachMorph isExpanded.
	eachMorph children ifEmpty: [ ^ self ]. 
	tag := compiledMethod package classTagForClass: compiledMethod methodClass.
	tagMorph := eachMorph children select: [ :eachChild | eachChild complexContents item = tag ].
	self assert: tagMorph size equals: 1.
	^ self assert: tagMorph first isSelected
]

{ #category : #asserting }
PackageTreeNautilusTest >> assertPackageWidget [
	| packageNodeList packageMorphList |
	packageNodeList := nautilus ui packageWidget packagesList nodeList
		select: [ :eachNode | eachNode item = compiledMethod package ].
	packageMorphList := nautilus ui packageWidget packagesList scroller submorphs
		select: [ :eachMorph | packageNodeList anySatisfy: [ :eachNode | eachMorph complexContents = eachNode ] ].
	self assert: packageNodeList size equals: packageMorphList size.
	self assert: packageNodeList size <= 1.
	packageMorphList do: [ :eachMorph | self assertPackageMorph: eachMorph ]
]

{ #category : #asserting }
PackageTreeNautilusTest >> assertSelectedCompiledMethod [
	| aText |
	self 
		assert: nautilus selectedPackage package 
			equals: compiledMethod package;
		assert: nautilus selectedPackage item categoryName 
			equals: compiledMethod methodClass category;
		assert: nautilus selectedClass
			equals: compiledMethod methodClass;
		assert: nautilus selectedCategory 
			equals: expectedMethodCategory;
		assert: nautilus selectedMethod 
			equals: expectedCompiledMethod;
		assert: nautilus ui packageWidget selectedPackage package 
			equals: compiledMethod package;
		assert: nautilus ui packageWidget selectedPackage item categoryName 
			equals: compiledMethod methodClass category;
		assert: nautilus ui classWidget selectedClass 
			equals: compiledMethod methodClass;
		assert: nautilus ui methodWidget selectedMethods asArray 
			equals: ((Array with: expectedCompiledMethod) copyWithout: nil).

	aText := nautilus ui sourceCodeContainer submorphs first submorphs first text asString.
	nautilus selectedMethod 
		ifNotNil: [ self assert: aText equals: compiledMethod sourceCode ].
		
	self assertPackageWidget.
	self assertMethodWidget.
]

{ #category : #accessing }
PackageTreeNautilusTest >> compiledMethod: aCompiledMethod [
	compiledMethod := aCompiledMethod.
	expectedMethodCategory := compiledMethod category.
	expectedCompiledMethod := compiledMethod.
]

{ #category : #running }
PackageTreeNautilusTest >> setUp [
	super setUp.
	self compiledMethod: (NautilusUI>>#addClassIn:).
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
]

{ #category : #running }
PackageTreeNautilusTest >> tearDown [
	nautilus ifNotNil: [ :aNautilus | aNautilus ui ifNotNil: [ :aUI | aUI close ] ].
	lastValue ifNotNil: [ :aValue | SharedPackageFilterPatternsHolder instance lastValue: aValue ].
	super tearDown
]

{ #category : #tests }
PackageTreeNautilusTest >> testFullBrowseLastValueIsNil [
	| newNautilus |
	self testOpenOnMethodLastValueIsNil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testFullBrowseLastValueNotNil [
	| newNautilus |
	self testOpenOnMethodLastValueNotNil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testFullBrowseLastValueNotNil2 [
	| newNautilus |
	self compiledMethod: self class >> #testFullBrowseLastValueNotNil.
	self testOpenOnMethodLastValueNotNil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testFullBrowseLastValueNotNilSelectedCategoryIsNil [
	| newNautilus |
	self compiledMethod: self class >> #testFullBrowseLastValueNotNil.
	self testOpenOnMethodLastValueNotNil.
	nautilus selectedCategory: nil.
	nautilus selectedMethod: compiledMethod. 
	self assert: nautilus selectedCategory isNil.
	self assert: nautilus selectedMethod equals: compiledMethod.
	expectedMethodCategory := nil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testFullBrowseOnClass [
	| newNautilus |
	self testOpenOnMethodLastValueIsNil.
	nautilus ui browseClass: compiledMethod methodClass.
	expectedMethodCategory := nil.
	expectedCompiledMethod := nil.
	newNautilus := nautilus ui fullBrowse.
	nautilus close.
	nautilus := newNautilus.
	self assertSelectedCompiledMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testOpen [
	| allPackages filteredPackages |
	SharedPackageFilterPatternsHolder instance lastValue: nil.
	nautilus := Nautilus open.
	allPackages := nautilus packages copy.  
	allPackages := nautilus ui packageWidget packagesList nodeList copy.
	nautilus packagePatternString: 'H'.
	filteredPackages := nautilus ui packageWidget packagesList nodeList copy.
	self assert: allPackages size > filteredPackages size.

]

{ #category : #tests }
PackageTreeNautilusTest >> testOpenLastValueIsNil [
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: nil.
	nautilus := Nautilus open.
	nautilus packagePatternString: 'H'.
	nautilus packagePatternString: 'anything long'.
	nautilus packagePatternString: ''.

]

{ #category : #tests }
PackageTreeNautilusTest >> testOpenLastValueNotNil [
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: 'K'.
	nautilus := Nautilus open.
	nautilus packagePatternString: 'H'.
	nautilus packagePatternString: 'anything long'.
	nautilus packagePatternString: ''.

]

{ #category : #tests }
PackageTreeNautilusTest >> testOpenOnMethod [
	nautilus := Nautilus openOnMethod: compiledMethod.
	self assertSelectedCompiledMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testOpenOnMethodLastValueIsNil [
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: nil.
	self testOpenOnMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testOpenOnMethodLastValueNotNil [
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: 'AAATest'.
	self testOpenOnMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testPackagePatternString [
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: ''.
	nautilus := Nautilus openOnMethod: compiledMethod.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: 'H'.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: 'anything long'.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: ''.
	self assertSelectedCompiledMethod.
]

{ #category : #tests }
PackageTreeNautilusTest >> testPackagePatternStringRootPackageTag [
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: ''.
	self compiledMethod: (Object>>#=).
	nautilus := Nautilus openOnMethod: compiledMethod.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: 'H'.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: 'anything long'.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: ''.
	self assertSelectedCompiledMethod.
]
