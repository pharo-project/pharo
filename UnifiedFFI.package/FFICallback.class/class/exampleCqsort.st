examples
exampleCqsort
	| cb rand values orig sort |

	rand := Random new.
	
	values := FFIExternalArray externalNewType: 'double' size: 100.
	1 to: 100 do: [ :i| values at: i put: rand next ].
	orig := (1 to: 100) collect: [:i| values at: i] as: Array.
	cb := FFICallback
		signature:  #(int (const void *arg1, const void *arg2))
		block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign ].
	[
		self primQsort: values with: 100 with: values type typeSize with: cb.
		sort := values asArray ]
	ensure: [ values free ].
	
	^orig -> sort