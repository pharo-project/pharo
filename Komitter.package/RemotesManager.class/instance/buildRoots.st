private
buildRoots
	^ MCRepositoryGroup default repositories
		select: [ :each | each isCache not ]
		thenCollect: [ :each || remote  |			
			remote := each koRemote.
			TreeNodeModel new
				content: remote;
				hasContentToShow: true;
				selected: ( selectedRemotes includes: each );
				whenSelectedChanged: [ :bool | self selected: each changed: bool ] ;
				yourself ]