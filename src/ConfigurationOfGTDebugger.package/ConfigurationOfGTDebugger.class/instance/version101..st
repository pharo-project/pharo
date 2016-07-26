versions
version101: spec
	<version: '1.0.1' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.0.1'.
		spec author: 'AndreiChis'.
		spec timestamp: '05/05/2014'.
		spec 
			project: 'GTInspector' with: #'stable';
			project: 'NewDebugger' with: #'stable'.
		spec 
			package: #'GT-Debugger' with: 'GT-Debugger-AndreiChis.240'].
