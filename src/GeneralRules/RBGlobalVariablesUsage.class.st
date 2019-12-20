"
Global Variables are a known design smell. 

Specially in Pharo, we do not want to keep global bindings because their usage can a will produce undesirable implicit coupling, avoiding out language to evolve into the usage of powerful modules. 
"
Class {
	#name : #RBGlobalVariablesUsage,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #'testing-interest' }
RBGlobalVariablesUsage class >> checksMethod [
	^ true
]

{ #category : #running }
RBGlobalVariablesUsage >> check: aMethod forCritiquesDo: aCriticBlock [
	"Beware that ClassVariables respond true to #isGlobal. 
	 The trick with 'ifAbsent:[Object] is basically a way not select ClassVariables (or Pools)"
	| problemSends  |
	problemSends := 
		(aMethod ast allChildren select: [ :node | node isVariable ]) 
			select: [:varNode | 
				varNode isGlobal and: [(Smalltalk globals at: varNode name ifAbsent: [Object]) isClass not] ].
	
	problemSends do: [ :varNode |
		aCriticBlock cull: (self createTrivialCritiqueOn: aMethod intervalOf: varNode hint: varNode name)]
]

{ #category : #running }
RBGlobalVariablesUsage >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBGlobalVariablesUsage >> name [
	^ 'Global variable usage'
]

{ #category : #running }
RBGlobalVariablesUsage >> rationale [
	^ 'http://wiki.c2.com/?GlobalVariablesAreBad
	
	Why Global Variables Should Be Avoided When Unnecessary

    Non-locality -- Source code is easiest to understand when the scope of its individual elements are limited. Global variables can be read or modified by any part of the program, making it difficult to remember or reason about every possible use.
    No Access Control or Constraint Checking -- A global variable can be get or set by any part of the program, and any rules regarding its use can be easily broken or forgotten. (In other words, get/set accessors are generally preferable over direct data access, and this is even more so for global data.) By extension, the lack of access control greatly hinders achieving security in situations where you may wish to run untrusted code (such as working with 3rd party plugins).
    Implicit coupling -- A program with many global variables often has tight couplings between some of those variables, and couplings between variables and functions. Grouping coupled items into cohesive units usually leads to better programs.
    Concurrency issues -- if globals can be accessed by multiple threads of execution, synchronization is necessary (and too-often neglected). When dynamically linking modules with globals, the composed system might not be thread-safe even if the two independent modules tested in dozens of different contexts were safe.
    Namespace pollution -- Global names are available everywhere. You may unknowingly end up using a global when you think you are using a local (by misspelling or forgetting to declare the local) or vice versa. Also, if you ever have to link together modules that have the same global variable names, if you are lucky, you will get linking errors. If you are unlucky, the linker will simply treat all uses of the same name as the same object.
    Memory allocation issues -- Some environments have memory allocation schemes that make allocation of globals tricky. This is especially true in languages where "constructors" have side-effects other than allocation (because, in that case, you can express unsafe situations where two globals mutually depend on one another). Also, when dynamically linking modules, it can be unclear whether different libraries have their own instances of globals or whether the globals are shared.
    Testing and Confinement - source that utilizes globals is somewhat more difficult to test because one cannot readily set up a ''clean'' environment between runs. More generally, source that utilizes global services of any sort (e.g. reading and writing files or databases) that aren''t explicitly provided to that source is difficult to test for the same reason. For communicating systems, the ability to test system invariants may require running more than one ''copy'' of a system simultaneously, which is greatly hindered by any use of shared services - including global memory - that are not provided for sharing as part of the test. 
	
	'
]

{ #category : #running }
RBGlobalVariablesUsage >> severity [
	^ #warning
]
