versions
version221: spec
	<version: '2.2.1' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Changes in the use of #garbageCollect'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/25/2015 18:00'.
		spec project: 'GlamourCore' with: '3.4.1'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.351';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.185';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.49'. ].
