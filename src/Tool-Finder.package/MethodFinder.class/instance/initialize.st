initialize
initialize
	"The methods we are allowed to use.  (MethodFinder new initialize) "
	super initialize.

	Approved := Set new.
	AddAndRemove := Set new.
	Blocks := Set new.
	"These modify an argument and are not used by the MethodFinder: longPrintOn: printOn: storeOn: sentTo: storeOn:base: printOn:base: absPrintExactlyOn:base: absPrintOn:base: absPrintOn:base:digitCount: writeOn: writeScanOn: possibleVariablesFor:continuedFrom: printOn:format:"

"Object"  
	#("in class, instance creation"  newFrom: 
"accessing" at: basicAt: basicSize in: size yourself 
"testing" ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isColor isFloat isFraction  isInteger isMorph isNil isNumber isPoint isText isTransparent isWebBrowser notNil pointsTo: wantsSteps 
"comparing" = == hash identityHash ~= ~~ 
"copying" copy shallowCopy 
"dependents access" canDiscardEdits dependents hasUnacceptedEdits 
"updating" changed changed: okToChange update: windowIsClosing 
"printing" fullPrintString isLiteral longPrintString printString storeString 
"class membership" class isKindOf: isMemberOf: respondsTo:  
"error handling" 
"user interface"defaultBackgroundColor defaultLabel fullscreenSize initialExtent modelWakeUp  
"system primitives" instVarAt: instVarNamed: 
"private" 
"associating" -> 
"converting" as: asOrderedCollection asString 
"casing" caseOf: caseOf:otherwise: 
"binding" bindingOf: 
"macpal"  currentEvent currentHand currentWorld flash   
"flagging" flag: 
"translation support" "objects from disk" "finalization" ) do: [:sel | Approved add: sel].
	#(at:add: at:put: basicAt:put: "NOT instVar:at:"
"message handling" perform: perform:orSendTo: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: 
) do: [:sel | AddAndRemove add: sel].

"Boolean, True, False, UndefinedObject"  
	#("logical operations" & eqv: not xor: |
"controlling" and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or:
"copying" 
"testing" isEmptyOrNil) do: [:sel | Approved add: sel].

"Behavior" 
	#("initialize-release"
"accessing"  format methodDict sourceCodeTemplate subclassDefinerClass
"testing" instSize instSpec isBits isBytes isFixed isPointers isVariable isWeak isWords
"copying"
"printing" printHierarchy
"creating class hierarchy"
"creating method dictionary"
"instance creation" basicNew basicNew: new new:
"accessing class hierarchy" allSubclasses allSubclassesWithLevelDo:startingLevel: allSuperclasses subclasses superclass withAllSubclasses withAllSuperclasses
"accessing method dictionary" allSelectors compiledMethodAt: compiledMethodAt:ifAbsent: firstCommentAt: lookupSelector: selectors selectorsDo: selectorsWithArgs: "slow but useful ->" sourceCodeAt: sourceCodeAt:ifAbsent: sourceMethodAt: sourceMethodAt:ifAbsent:
"accessing instances and variables" allClassVarNames allInstVarNames allSharedPools classVarNames instVarNames instanceCount sharedPools someInstance subclassInstVarNames
"testing class hierarchy" inheritsFrom: kindOfSubclass
"testing method dictionary" canUnderstand: classThatUnderstands: hasMethods includesSelector: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsStoreInto:) do: [:sel | Approved add: sel].

"ClassDescription"
	#("initialize-release" 
"accessing"  isMeta name theNonMetaClass
"copying" 
"printing" classVariablesString instanceVariablesString sharedPoolsString
"instance variables"  
"method dictionary" 
"organization" category organization whichCategoryIncludesSelector:
"compiling" acceptsLoggingOfCompilation wantsChangeSetLogging
"fileIn/Out" definition
"private" ) do: [:sel | Approved add: sel].

"Class"
	#("initialize-release" 
"accessing" classPool
"testing"
"copying" 
"class name" 
"instance variables" 
"class variables" classVarAt: classVariableAssociationAt:
"pool variables" 
"compiling" 
"subclass creation" 
"fileIn/Out" ) do: [:sel | Approved add: sel]. 

"Metaclass"
	#("initialize-release" 
"accessing" isSystemDefined soleInstance
"copying" "instance creation" "instance variables"  "pool variables" "class hierarchy"  "compiling"
"fileIn/Out"  nonTrivial ) do: [:sel | Approved add: sel].

"Context, BlockContext"
	#(receiver client method receiver tempAt: 
"debugger access" pc selector sender shortStack sourceCode tempNames tempsAndValues
"controlling"  "printing" "system simulation" 
"initialize-release" 
"accessing" hasMethodReturn home numArgs
"evaluating" value value:ifError: value:value: value:value:value: value:value:value:value: valueWithArguments:
"controlling"  "scheduling"  "instruction decoding"  "printing" "private"  "system simulation" ) do: [:sel | Approved add: sel].
	#(value: "<- Association has it as a store" ) do: [:sel | AddAndRemove add: sel].

"Message"
	#("inclass, instance creation" selector: selector:argument: selector:arguments:
"accessing" argument argument: arguments sends:
"printing" "sending" ) do: [:sel | Approved add: sel].
	#("private" setSelector:arguments:) do: [:sel | AddAndRemove add: sel].

"Magnitude"
	#("comparing" < <= > >= between:and:
"testing" max: min: min:max: ) do: [:sel | Approved add: sel].

"Date, Time"
	#("in class, instance creation" fromDays: fromSeconds: fromString: today
	"in class, general inquiries" dayOfWeek: daysInMonth:forYear: daysInYear: firstWeekdayOfMonth:year: indexOfMonth: nameOfDay: nameOfMonth:
"accessing" day monthIndex monthName weekday year
"arithmetic" addDays: subtractDate: subtractDays:
"comparing"
"inquiries" dayOfMonth daysInMonth daysInYear daysLeftInYear firstDayOfMonth previous:
"converting" asSeconds
"printing"  mmddyyyy printFormat: 
"private" weekdayIndex 
	"in class, instance creation" fromSeconds: now 
	"in class, general inquiries" dateAndTimeFromSeconds: millisecondClockValue millisecondsToRun: totalSeconds
"accessing" hours minutes seconds
"arithmetic" addTime: subtractTime:
"comparing"
"printing" intervalString print24 
"converting") do: [:sel | Approved add: sel].
	#("private" 
		 ) do: [:sel | AddAndRemove add: sel].

"Number"
	#("in class" readFrom:base: 
"arithmetic" * + - / // \\ abs negated quo: reciprocal rem:
"mathematical functions" arcCos arcSin arcTan arcTan: cos exp floorLog: ln log log: raisedTo: raisedToInteger: sin sqrt squared tan
"truncation and round off" ceiling detentBy:atMultiplesOf:snap: floor roundTo: roundUpTo: rounded truncateTo: truncated
"comparing"
"testing" even isDivisibleBy: isInfinite isNaN isZero negative odd positive sign strictlyPositive
"converting" @ asInteger asNumber asPoint asSmallAngleDegrees degreesToRadians radiansToDegrees
"intervals" to: to:by: 
"printing" printStringBase: storeStringBase: ) do: [:sel | Approved add: sel].

"Integer"
	#("in class" primesUpTo:
"testing" isPowerOfTwo
"arithmetic" alignedTo:
"comparing"
"truncation and round off" normalize
"enumerating" timesRepeat:
"mathematical functions" degreeCos degreeSin factorial gcd: lcm: take:
"bit manipulation" << >> allMask: anyMask: bitAnd: bitClear: bitInvert bitInvert32 bitOr: bitShift: bitXor: lowBit noMask:
"converting" asCharacter asColorOfDepth: asFloat asFraction asHexDigit
"printing" asStringWithCommas hex hex8 radix:
"system primitives" lastDigit replaceFrom:to:with:startingAt:
"private" "benchmarks" ) do: [:sel | Approved add: sel].

"SmallInteger, LargeNegativeInteger, LargePositiveInteger"
	#("arithmetic" "bit manipulation" highBit "testing" "comparing" "copying" "converting" "printing" 
"system primitives" digitAt: digitLength 
"private" ) do: [:sel | Approved add: sel].
	#(digitAt:put: ) do: [:sel | AddAndRemove add: sel].

"Float"
	#("arithmetic"
"mathematical functions" reciprocalLogBase2 timesTwoPower:
"comparing" "testing"
"truncation and round off" exponent fractionPart integerPart significand significandAsInteger
"converting" asApproximateFraction asIEEE32BitWord asTrueFraction
"copying") do: [:sel | Approved add: sel].

"Fraction, Random"
	#(denominator numerator reduced next nextValue) do: [:sel | Approved add: sel].
	#(setNumerator:denominator:) do: [:sel | AddAndRemove add: sel].

"Collection"
	#("accessing" anyOne
"testing" includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isEmpty isSequenceable occurrencesOf:
"enumerating" collect: collect:thenSelect: count: detect: detect:ifNone: detectMax: detectMin: detectSum: inject:into: reject: select: select:thenCollect: intersection:
"converting" asBag asCharacterSet asSet  asSortedCollection asSortedCollection:
"printing"
"private" maxSize
"arithmetic"
"math functions" average max median min range sum) do: [:sel | Approved add: sel].
	#("adding" add: addAll: addIfNotPresent:
"removing" remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat: remove:ifAbsent:) do: [:sel | AddAndRemove add: sel].

"SequenceableCollection"
	#("comparing" hasEqualElements:
"accessing" allButFirst allButLast at:ifAbsent: atAll: atPin: atWrap: fifth first first: fourth identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt:ifAbsent: indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last second sixth third
"removing"
"copying" , copyAfterLast: copyAt:put: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpTo: copyUpToLast: copyWith: copyWithout: copyWithoutAll: forceTo:paddingWith: shuffled sort:
"enumerating" collectWithIndex: findFirst: findLast: pairsCollect: with:collect: withIndexCollect: polynomialEval:
"converting" asArray asDictionary asFloatArray asIntegerArray asStringWithCr asWordArray reversed
 ) do: [:sel | Approved add: sel].
	#( swap:with:) do: [:sel | AddAndRemove add: sel].

"ArrayedCollection, Bag"
	#("private" defaultElement 
"sorting" isSorted
"accessing" cumulativeCounts sortedCounts sortedElements "testing" "adding" add:withOccurrences: "removing" "enumerating" 
	) do: [:sel | Approved add: sel].
	#( mergeSortFrom:to:by: sort sort: add: add:withOccurrences:
"private" setDictionary ) do: [:sel | AddAndRemove add: sel].

"Other messages that modify the receiver"
	#(atAll:put: atAll:putAll: atAllPut: atWrap:put: replaceAll:with: replaceFrom:to:with:  removeFirst removeLast) do: [:sel | AddAndRemove add: sel].

	self initialize2.

"
MethodFinder new initialize.
MethodFinder new organizationFiltered: Set
"

