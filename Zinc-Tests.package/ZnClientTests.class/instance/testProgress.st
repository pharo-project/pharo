testing
testProgress
	"[ :bar |
		bar title: 'Downloading Sources...'.
		[
			ZnClient new 
				url: 'http://files.pharo.org/sources/PharoV30.sources'; 
				signalProgress: true;	
				downloadTo: FileLocator temp ]
			on: HTTPProgress 
			do: [ :progress |
				progress isEmpty ifFalse: [ bar current: progress percentage ]. 
				progress resume ] ] asJob run."
		
	self 
		should: [ 
			ZnClient new
				beOneShot;
				signalProgress: true;
				get: self smallHtmlUrl ] 
		raise: HTTPProgress
