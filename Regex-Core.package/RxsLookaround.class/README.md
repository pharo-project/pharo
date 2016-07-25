I lookaround is used for lookaheads and lookbehinds. They are used to check if the input matches a certain subexpression without consuming any characters (e.g. not advancing the match position).

Lookarounds can be positive or negative. If they are positive the condition fails if the subexpression fails, if they are negative it is inverse.