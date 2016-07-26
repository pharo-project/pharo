testing
showWithDebugger: aDebugger

	^ SlReporter enabled & (self autoSubmitWithDebugger: aDebugger) not