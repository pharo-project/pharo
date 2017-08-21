accessing
next: anInteger putAll: aCollection startingAt: startIndex


        (self isBinary or: [ aCollection class == ByteArray ]) ifTrue: [
                ^super next: anInteger putAll: aCollection startingAt: startIndex ].
        ^self converter next: anInteger putAll: aCollection startingAt: startIndex toStream: self