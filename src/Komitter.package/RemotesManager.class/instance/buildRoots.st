private
buildRoots
	^ MCRepositoryGroup default repositories
		select: [ :each | (each isCache not) and: [ each isRemote ] ]
		thenCollect: [ :each | | remote  |			
			remote := each koRemote.
			TreeNodePresenter new
				content: remote;
				hasContentToShow: true;
				selected: ( selectedRemotes includes: each );
				whenSelectedChanged: [ :bool | self selected: each changed: bool ] ;
				yourself ]