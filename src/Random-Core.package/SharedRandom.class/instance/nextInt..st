accessing
nextInt: anInteger
    ^ mutex critical: [ super nextInt: anInteger ].