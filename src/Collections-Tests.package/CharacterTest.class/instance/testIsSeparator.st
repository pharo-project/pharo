tests
testIsSeparator
    {Character space.
    Character cr.
    Character lf.
    Character newPage.
    Character tab} do: [ :each | self assert: each isSeparator ].
    Character alphabet do: [ :each | self deny: each isSeparator ]