comparing
hash

	^ origin hash bitXor: (target hash bitXor: linkBuilder hash)