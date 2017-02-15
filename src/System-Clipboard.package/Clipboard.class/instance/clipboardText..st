accessing
clipboardText: text 

        | string |
        string := text asString.
        self noteRecentClipping: text asText copyWithoutExternalReferences.
        contents := text asText copyWithoutExternalReferences.
        string := string convertToWithConverter: UTF8TextConverter new.
        self primitiveClipboardText: string.