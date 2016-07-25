other
next: anInteger
    "Answer an Array of the next anInteger objects in the stream."
    | array |

    array := Array new: anInteger.
    1 to: anInteger do: [:i |
        array at: i put: self next].
    ^ array