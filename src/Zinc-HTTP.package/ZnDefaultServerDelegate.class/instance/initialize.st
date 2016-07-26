initialization
initialize
	self
		map: 'welcome' to: #welcome:;
		map: 'help' to: #help:;
		map: 'echo' to: #echoRequest:;
		map: 'small' to: #small:;
		map: 'dw-bench' to: #dwbench:;
		map: 'favicon.ico' to: #favicon:;
		map: 'random' to: #random:;
		map: 'bytes' to: #bytes:;
		map: 'status' to: #status:;
		map: 'unicode' to: #unicode:;
		map: 'session' to: #sessionRequest:;
		map: 'error' to: #errorResponse:;
		map: 'form-test-1' to: #formTest1:;
		map: 'form-test-2' to: #formTest2:;
		map: 'form-test-3' to: #formTest3:;
		map: '/' to: 'welcome'