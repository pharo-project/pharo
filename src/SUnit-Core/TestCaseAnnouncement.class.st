"
This is a common superclass for announcements related to SUnit test cases.

Instance Variables
        testCase:                       <TestCase>
        testResult:                     <TestResult>
        testSelector:           	   <Symbol>

testCase
        - The test case

testResult
        - The result of the test case

testSelector
        - The selector of the test method
"
Class {
	#name : #TestCaseAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'testCase',
		'testSelector',
		'testResult'
	],
	#category : #SUnit-Core-Announcements
}

{ #category : #testing }
TestCaseAnnouncement >> hasErrors [
        "Return true if the test result has errors."
        
        ^testResult hasErrors
]

{ #category : #testing }
TestCaseAnnouncement >> hasFailures [
        "Return true if the test result has failures."
        
        ^testResult hasFailures
]

{ #category : #testing }
TestCaseAnnouncement >> hasPassed [
        "Return true if the test has passed."
        
        ^testResult hasPassed
]

{ #category : #accessing }
TestCaseAnnouncement >> testCase [
        "Returns the test case."
        
        ^testCase
]

{ #category : #accessing }
TestCaseAnnouncement >> testCase: aTestCase [
        "Sets the test case to the given test case."
        
        testCase := aTestCase
]

{ #category : #accessing }
TestCaseAnnouncement >> testResult [
        "Returns the test result."
        
        ^testResult
]

{ #category : #accessing }
TestCaseAnnouncement >> testResult: aTestResult [
        "Sets the test result to the given test result."
        
        testResult := aTestResult
]

{ #category : #accessing }
TestCaseAnnouncement >> testSelector [
        "Returns the test selector."
        
        ^testSelector
]

{ #category : #accessing }
TestCaseAnnouncement >> testSelector: aSymbol [
        "Sets the test selector to the given symbol."
        
        testSelector := aSymbol
]
