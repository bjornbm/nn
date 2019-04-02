+   `nnlast` command to edit the last note created
    +   or `nne` with no arguments?
+   `nn rename` to change the title
+   `nn retag` to change the tag
-   Multiple tags?
-   Parse IDs in option parsing (gives better errors?)
-   Fix "latest" behavior in SelectOne to be consistent with SelectMany
    -   Or can SelectOne be retired?
-   Add generic options? (`Options` in app/Options.hs currently not used)
-   `nn edit` should select only last item. Now selects all (like list) and tries to edit all. Oops!
    -   The same is true for all (should default to --last) except `list`! But should it be possible to select all for `cat` for example??
-   Compile using `-Wall` (or opinionated subset).
-   Commands in `Main` should not be different (partial) functions.
-   Use `Safe` library and replace all partial functions.

