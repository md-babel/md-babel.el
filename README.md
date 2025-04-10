# `md-babel.el`: Execute block-at-point in `markdown-mode`

Brings the joy of org-mode babel code block execution to Markdown documents in Emacs using [md-babel](https://md-babel.org)

## Usage

Create a key binding for `md-babel-execute-block-at-point`.
In `markdown-mode`, <kbd>C-c C-c</kbd> is already bound to `markdown-mode-command-map`, from which you can export and check the document. 
I suggest you add another <kbd>C-c</kbd> to it, so that you can press this key combination 3 times (instead of org's 2):

```elisp
(define-key markdown-mode-command-map (kbd "C-c") #'md-babel-execute-block-at-point)
```

Now you can put your point into any GitHub-flavored Markdown code block and evaluate it.

## Example 

For example, evaluating the Emacs Lisp block above, which (repeated literally) is:

    ```elisp
    (define-key markdown-mode-command-map (kbd "C-c") #'md-babel-execute-block-at-point)
    ```

... will produce a JSON response from the `md-babel` program like this:

```json
{
  "range": {
    "from": { "line": 12, "column": 1 },
    "to": { "line": 12, "column": 1 }
  },
  "error": "Configuration missing to execute code with language “elisp”.",
  "replacementRange": {
    "from": { "line": 11, "column": 1 },
    "to": { "line": 19, "column": 4 }
  },
  "replacementString": "```elisp\n(define-key markdown-mode-command-map (kbd \"C-c\") #'md-babel-execute-block-at-point)\n```\n\n<!--Error:-->\n```\nConfiguration missing to execute code with language “elisp”.\n```"
}
```

The `"replacementString"` repeats the original code block and appends an error block, meant to replace the buffer substring denoted by `"replacementRange"`, so that it is inserted into the same buffer. The error block will look like this:

    <!--Error:-->
    ```
    Configuration missing to execute code with language “elisp”.
    ```

Well, that's a bummer, so ...

### Adding Elisp Support to `md-babel`

... here's how you can teach `md-babel` to run your Emacs Lisp code.

Create a configuration JSON file with this content in `~/.config/md-babel/config.json`

```json
{
  "elisp": {
    "path": "/usr/bin/env",
    "defaultArguments": ["emacsclient", "--eval"]
  }
}
```

This assumes you're running Emacs server. 

> [!NOTE]  
> Emacs server allows you to evaluate Emacs Lisp code in the context of your currently running instance. 
> That is required so that evaluating the code block with the `define-key` function call actually affects the Emacs instance you're currently running like <kbd>M-x eval-expression</kbd>.

Then try to run the code block with <kbd>M-x md-babel-execute-block-at-point</kbd>.

If you want to execute a code block in a self-contained Emacs that doesn't depend on your config, use batch execution mode instead:

```json
{
  "elisp": {
    "path": "/usr/bin/env",
    "defaultArguments": ["emacs", "--batch", "--eval"]
  }
}
```

With that setting, you can execute this code block and get the message from standard output routed into your result block:

```elisp
(message "Hello, Emacs!")
```
