tests - parsing
testParsingFromVersionString
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