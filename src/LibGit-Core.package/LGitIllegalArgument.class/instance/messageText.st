accessing
messageText
	^ 'libgit2 ffi call expects a(n) {1} but instead received {2}'
			format: {self expectedClass. self object}
