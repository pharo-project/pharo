updating
update
	updateProcess ifNotNil: [ updateProcess terminate ].
	updateProcess := [ |roots| 
			roots := self roots.
			UIManager default defer: [ self tree roots: roots ].
		] fork.