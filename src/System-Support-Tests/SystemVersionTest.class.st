"
SUnit tests for SystemVersion
"
Class {
	#name : #SystemVersionTest,
	#superclass : #TestCase,
	#category : #'System-Support-Tests'
}

{ #category : #utilities }
SystemVersionTest >> checkIsCompatibleWithResultOfVersionMessage: aString [
	"Checks whether a string is compatible with the result of the existing #version message who returned 
	   Pharomajor.minor[suffix]
	"
	
	| rx |
	rx := '(Pharo)([0-9]+)[.]([0-9]+)([a-z]+)' asRegex.
	self 
		assert: (rx matches: aString);
		assert: (rx subexpression: 2) equals: 'Pharo';
		assert: (rx subexpression: 3) asNumber > 0;
		assert: (rx subexpression: 4) asNumber >= 0;
		assert: (rx subexpression: 5) notEmpty
]

{ #category : #tests }
SystemVersionTest >> testCalculationOfHighestUpdate [

	self assert: (SystemVersion fromVersionString: 'Pharo-6.0+alpha.build.510') highestUpdate equals: 60510 
]

{ #category : #tests }
SystemVersionTest >> testCommitHashShort [

	| version |
	version := SystemVersion fromVersionString: 'Pharo-7.0+alpha.build.2.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	self assert: version commitHashShort equals: '368dcda'
]

{ #category : #'tests - compatibility' }
SystemVersionTest >> testDoNotBreakSmalltalkVersion [
	"TODO: This one might be yellow when we still set the suffix with build nr and hash from the outside."
	
	self checkIsCompatibleWithResultOfVersionMessage: Smalltalk version 
	
]

{ #category : #tests }
SystemVersionTest >> testDottedMajorMinor [ 
	 
	self assert: (SystemVersion fromVersionString: 'Squeak3.7+alpha') dottedMajorMinor = '3.7'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.0+ALPHA') dottedMajorMinor = '1.0'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.1') dottedMajorMinor = '1.1'.
]

{ #category : #'tests - instance creation' }
SystemVersionTest >> testInstanceCreation [

	| version |
	version := (SystemVersion new).
	self 
		assert: version type equals: 'Pharo';
		assert: version major equals: 0;
		assert: version minor equals: 0;
		assert: version suffix isEmpty;
		assert: version build equals: SystemVersion invalidBuildNumber;	
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;
		assert: version commitHashShort isEmpty;		
		assert: (version printString beginsWith: 'Pharo-0.0')
]

{ #category : #'tests - instance creation' }
SystemVersionTest >> testInstanceCreationMajorMinor [

	| version |
	version := (SystemVersion major: 6 minor: 1).
	self 
		assert: version type equals: 'Pharo';
		assert: version major equals: 6;
		assert: version minor equals: 1;
		assert: version suffix isEmpty;
		assert: version build equals: SystemVersion invalidBuildNumber;
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;		
		assert: (version printString beginsWith: 'Pharo-6.1')
]

{ #category : #'tests - instance creation' }
SystemVersionTest >> testInstanceCreationMajorMinorSuffix [

	| version |
	version := (SystemVersion major: 7 minor: 0 suffix: 'alpha').
	self 
		assert: version type equals: 'Pharo';
		assert: version major equals: 7;
		assert: version minor equals: 0;
		assert: version suffix equals: 'alpha';
		assert: version build equals: SystemVersion invalidBuildNumber;
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;		
		assert: (version printString beginsWith: 'Pharo-7.0+alpha')
]

{ #category : #'tests - instance creation' }
SystemVersionTest >> testInstanceCreationMajorMinorSuffixBuild [

	| version |
	version := (SystemVersion major: 7 minor: 0 suffix: 'alpha' build: 142).
	self 
		assert: version type equals: 'Pharo';	
		assert: version major equals: 7;
		assert: version minor equals: 0;
		assert: version suffix equals: 'alpha';
		assert: version build equals: 142;
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;		
		assert: (version printString beginsWith: 'Pharo-7.0+alpha.build.142')
]

{ #category : #'tests - instance creation' }
SystemVersionTest >> testInstanceCreationMajorMinorSuffixBuildCommitHash [

	| version |
	version := (SystemVersion major: 7 minor: 0 suffix: 'alpha' build: 148 commitHash: '368dcda1b1ba81215fb7355cfc1e901d45395bd8').
	self 
		assert: version type equals: 'Pharo';	
		assert: version major equals: 7;
		assert: version minor equals: 0;
		assert: version suffix equals: 'alpha';
		assert: version build equals: 148;
		assert: version commitHash equals: '368dcda1b1ba81215fb7355cfc1e901d45395bd8';
		assert: version commitHashShort equals: '368dcda';
		assert: (version printString beginsWith: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8')
]

{ #category : #'tests - instance creation' }
SystemVersionTest >> testInstanceCreationMajorMinorSuffixBuildWithoutSuffix [

	| version |
	version := (SystemVersion major: 7 minor: 0 suffix: '' build: 142).
	self 
		assert: version type equals: 'Pharo';	
		assert: version major equals: 7;
		assert: version minor equals: 0;
		assert: version suffix isEmpty;
		assert: version build equals: 142;
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;	
		assert: (version printString beginsWith: 'Pharo-7.0+build.142')
]

{ #category : #tests }
SystemVersionTest >> testMajorMinorVersion [ 
	 	
	self assert: (SystemVersion fromVersionString: 'Squeak3.7+alpha') majorMinorVersion = 'Squeak3.7'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.0+ALPHA') majorMinorVersion = 'Pharo1.0'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.1') majorMinorVersion = 'Pharo1.1'.
]

{ #category : #tests }
SystemVersionTest >> testMajorMinorWithSeparatingString [ 
	
	self assert: ((SystemVersion fromVersionString: 'Squeak3.7+alpha') majorMinor: '-') = '3-7'.
	self assert: ((SystemVersion fromVersionString: 'Pharo1.0+ALPHA') majorMinor: '')  = '10'.
	self assert: ((SystemVersion fromVersionString: 'Pharo1.1') majorMinor: '.') = '1.1'.
]

{ #category : #'tests - parsing' }
SystemVersionTest >> testParsingFromVersionString [
	"Test that each of the keys are parsable and return a printString giving as value of the associations"

	{'Squeak3.7+alpha' 		  -> 'Squeak-3.7+alpha'.
	 'Pharo1.0+ALPHA'  		  -> 'Pharo-1.0+ALPHA'.
	 'MyKillerApp-4.0+beta'    -> 'MyKillerApp-4.0+beta'.			
	 'Pharo0'           		  -> 'Pharo-0.0'.
	 'Pharo5'           		  -> 'Pharo-5.0'.
	 'Pharo6.0'         		  -> 'Pharo-6.0'.
	 'Pharo-7'          		  -> 'Pharo-7.0'.
	 'Pharo-7.0'        		  -> 'Pharo-7.0'.
	 'Pharo-7.0+alpha'  		  -> 'Pharo-7.0+alpha'.
	 'Pharo-7.0+alpha.build.0' -> 'Pharo-7.0+alpha.build.0'.
	 'Pharo-7.0+alpha.build.2' -> 'Pharo-7.0+alpha.build.2'.
	 'Pharo-7.0+alpha.build.2.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8' -> 'Pharo-7.0+alpha.build.2.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	  } 
	do: [:each | self assert: ((SystemVersion fromVersionString: each key) printString beginsWith: each value) ]
]

{ #category : #'tests - compatibility' }
SystemVersionTest >> testSmalltalkVersionEqualsCurrentVersion [
	"TODO: This one might be yellow when we still set the suffix with build nr and hash from the outside."
	
	self assert: Smalltalk version equals: SystemVersion current version
]

{ #category : #tests }
SystemVersionTest >> testSystemVersion [

	self assert: SystemVersion current notNil
]

{ #category : #tests }
SystemVersionTest >> testVersion [
	"Check that the #version method returns without a $- between Pharo and the major number."

	| version |
	version := SystemVersion fromVersionString: 'Pharo-7.0+alpha.build.2.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	self assert: version version equals: 'Pharo7.0alpha'
]

{ #category : #'tests - compatibility' }
SystemVersionTest >> testVersionIsCompatible [
	"Test that the result of the #version method follows the historic pattern (e.g. Pharo7.0alpha)
	 and is not broken"
	 
	self checkIsCompatibleWithResultOfVersionMessage: (SystemVersion major: 7 minor: 0 suffix: 'alpha' build: 148) version
]
