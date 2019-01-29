Class {
	#name : #GTSettingBrowserTest,
	#superclass : #TestCase,
	#category : #'GT-Tests-Spotter-SettingBrowser'
}

{ #category : #tests }
GTSettingBrowserTest >> testgroupedAndSortedSpotterExtensions [

	| res |
	res := GTSpotterExtensionSettings groupedAndSortedSpotterExtensions.
	self assert: res class equals: SortedCollection.
	self assert: (res allSatisfy: [ :ass | ass value allSatisfy: [ :g | g class = RGMethodDefinition ] ])
]

{ #category : #tests }
GTSettingBrowserTest >> testshouldSpotterIgnoreProcessorDefinedInMethod [
]
