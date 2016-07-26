accessing
next: anInteger into: anArray
    ^ mutex critical: [ super next: anInteger into: anArray ].