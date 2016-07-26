versions
version100: spec
	<version: '1.0.0' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.0.1'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/10/2014 14:11'.
		spec 
			project: 'GTInspector' with: #'stable';
			project: 'NewDebugger' with: #'stable'.
		spec 
			package: #'GT-Debugger' with: 'GT-Debugger-AndreiChis.213'].
