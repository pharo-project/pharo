Class {
	#name : #ExternalBrowserTest,
	#superclass : #TestCase,
	#instVars : [
		'initialWindows',
		'file'
	],
	#category : #'Tool-ExternalBrowser-Test'
}

{ #category : #running }
ExternalBrowserTest >> assert: serviceProvider dropFileWithSuffix: suffix [
	| allMatchingServices service |
	allMatchingServices := serviceProvider fileReaderServicesForFile: file pathString suffix: suffix.
	self deny: allMatchingServices isEmpty.
	service := allMatchingServices anyOne.
	self shouldnt: [service performServiceFor: file] raise: MessageNotUnderstood.

]

{ #category : #running }
ExternalBrowserTest >> classNameForTest [
	^#MyWonderfulClass
]

{ #category : #running }
ExternalBrowserTest >> fileForTestContent [
	^'''From Pharo7.0.0rc1 of 4 December 2018 [Build information: Pharo-7.0.0+rc1.build.1435.sha.4cd23cf7be1a90c7c52e9f8786860a6290b7c2a6 (64 Bit)] on 5 December 2018 at 9:30:34.922838 am''!
Object subclass: #', self classNameForTest,'
	instanceVariableNames: ''some iv''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: #MyCategory!
!', self classNameForTest,' commentStamp: ''NouryBouraqadi 12/4/2018 17:19'' prior: 0!
My unique and explicit comment!


!', self classNameForTest,' methodsFor: ''protocol'' stamp: ''NouryBouraqadi 12/4/2018 17:12''!
myMethod
	! !
'
]

{ #category : #running }
ExternalBrowserTest >> fileNameForTest [
	^ 'fileForExternalBrowserTest'
]

{ #category : #running }
ExternalBrowserTest >> initFileWithSuffix: suffix [
	file := FileSystem workingDirectory / self fileNameForTest, '.', suffix.
	file
		writeStreamDo: [ :stream | stream nextPutAll: self fileForTestContent ].
	initialWindows := World windowsSatisfying: [: each| true ].
]

{ #category : #running }
ExternalBrowserTest >> setUp [
	super setUp.
	initialWindows := World windowsSatisfying: [: each| true ].
]

{ #category : #running }
ExternalBrowserTest >> tearDown [ 
	| newWindows |
	file deleteIfAbsent: [ ].
	self class environment at: self classNameForTest  ifPresent: [ : cl | cl removeFromSystem ].
	newWindows := World windowsSatisfying: [ : each | (initialWindows includes: each) not ].
	newWindows do: #delete.
	super tearDown.
]

{ #category : #running }
ExternalBrowserTest >> testDropChangeFile [
	self initFileWithSuffix: 'changes'.
	self assert: ExternalChangesBrowser dropFileWithSuffix: 'changes'

]

{ #category : #running }
ExternalBrowserTest >> testDropStFile [
	self initFileWithSuffix: 'st'.
	self assert: ExternalBrowser dropFileWithSuffix: 'st'

]
