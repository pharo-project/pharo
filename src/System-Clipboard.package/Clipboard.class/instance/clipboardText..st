accessing
clipboardText: text 

        | string |
        string := text asString.
        self noteRecentClipping: text asText.
        contents := text asText.
        string := string convertToWithConverter: UTF8TextConverter new.
        self primitiveClipboardText: string.