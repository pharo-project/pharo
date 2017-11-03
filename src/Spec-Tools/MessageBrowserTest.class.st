"
A MessageBrowserTest is a test class for testing the behavior of MessageBrowser
"
Class {
	#name : #MessageBrowserTest,
	#superclass : #TestCase,
	#category : #Spec-Tools-Senders-Tests
}

{ #category : #tests }
MessageBrowserTest >> testFindingNotExistingText [

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #foo in: 'zork
hello world '.

	self assert: (intervalResult = (0 to: 0))
]

{ #category : #tests }
MessageBrowserTest >> testFindingText [

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #LayoutFrame in: 'foo
LayoutFrame

asLayoutFrame '.

	self assert: (intervalResult = (5 to: 15))
]

{ #category : #tests }
MessageBrowserTest >> testFindingTextWithColumn [

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #foo:bar: in: 'zork
self foo: 3 bar: 5.
self foo: 3 bar: 6 '.

	self assert: (intervalResult = (11 to: 21))
]

{ #category : #tests }
MessageBrowserTest >> testFindingTextWithOneColumn [

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #foo: in: 'zork
self foo: 3.
self foo: 6 '.

	self assert: (intervalResult = (11 to: 14))
]

{ #category : #'tests substrings' }
MessageBrowserTest >> testFindingTextWithoutJunkcharacter [

	| intervalResult |
	intervalResult := MessageBrowser new searchedString: #LayoutFrame in: 'foo
asLayoutFrame

LayoutFrame '.

	self assert: (intervalResult = (20 to: 30))
]
