adding
buildItemFor: aCandidateLink in: aProcessor
	|itemBrick|
	itemBrick := (aCandidateLink renderingProcessor render: aCandidateLink value)
			candidateLink: aCandidateLink;
			stepModel: self stepModel;
			yourself.
	^ itemBrick
	