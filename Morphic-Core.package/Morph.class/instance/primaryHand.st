structure
primaryHand

        | outer |
        outer := self outermostWorldMorph ifNil: [^ nil].
        ^ outer activeHand ifNil: [outer firstHand]