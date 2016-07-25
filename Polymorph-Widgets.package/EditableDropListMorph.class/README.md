I'm a DropListMorph which content can be modified.

Try:

(EditableDropListMorph
        on: [#(one two three)]
        list: #value
        selected: nil
        changeSelected: nil) openInWindow extent: 400@20.