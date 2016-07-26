Some classes have special importance to the core system.  DangerousClassNotifier notifies users which class defiitions should not be modified. 

This class is refactored from the following Pharo2 methods:

* Behaviour>>shouldNotBeRedefined

* ClassBuilder>>tooDangerousClasses

* ClassBuilder>>name:  inEnvironment:  subclassOf:  type:  instanceVariableNames:  classVariableNames:  poolDictionaries:  category:  unsafe: 

Instance Variables
	enabled:		<Boolean>
	restoreState:		<Boolean>

enabled
	- Specifies whether this checking is performed. Some system tests that check "dangerous" behaviour need this turned off. 

restoreState
	- After disabling for testing, need to restore the previous state. 
