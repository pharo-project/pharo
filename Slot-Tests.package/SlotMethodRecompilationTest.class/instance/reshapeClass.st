tests
reshapeClass
	^ class := self make: [ :builder | builder slots: #(#iv) ]