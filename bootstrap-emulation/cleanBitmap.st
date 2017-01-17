  
  | array bad obsoleteBitmap |

  array := Smalltalk specialObjectsArray copy.
  array at: 5 put: nil.
  Smalltalk specialObjectsArray becomeForward: array.

  obsoleteBitmap := SystemNavigation default obsoleteClasses detect: [ :c | c name = #AnObsoleteBitmap ].
  (Behavior classPool at: #ClassProperties) removeKey: obsoleteBitmap.
  ((Behavior classPool at: #ObsoleteSubclasses) at: ArrayedCollection put: (WeakArray ofSize: 3)).
  obsoleteBitmap instVarNamed: #layout put: nil.
  3 timesRepeat: [ Smalltalk garbageCollect. ].
  obsoleteBitmap class becomeForward: nil.
  bad := Set allSubInstances select: [:badSet | badSet isHealthy not ].
  bad do: #rehash.	
  bad := Dictionary allSubInstances select: [:dict | dict isHealthy not ].
  bad do: #rehash.	

  3 timesRepeat: [ Smalltalk garbageCollect ].
  Smalltalk globals flushClassNameCache.
  3 timesRepeat: [ 
  Smalltalk garbageCollect.
  Symbol compactSymbolTable ].
  HashedCollection rehashAll.
  SmalltalkImage current fixObsoleteReferences.
  3 timesRepeat: [ Smalltalk garbageCollect ].