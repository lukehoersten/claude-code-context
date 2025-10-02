# claude-code-context.el

Share your Emacs buffer context automatically with [Claude Code](https://github.com/anthropics/claude-code).

## Features

- **Automatic context sharing**: Automatically sends your current file, line number, column, and text selection to Claude Code
- **Flymake integration**: Optionally include linter/diagnostic output in the context
- **Idle-timer based**: Updates context only when Emacs is idle to avoid performance impact
- **Simple keybindings**: Easy-to-use commands for manual control

## Installation

### From MELPA (coming soon)

```elisp
(use-package claude-code-context
  :config
  (claude-code-context-mode 1))
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/lhoersten/claude-code-context.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/claude-code-context")
   (require 'claude-code-context)
   (claude-code-context-mode 1)
   ```

## Configuration

### Emacs Setup

Add to your `init.el`:

```elisp
(require 'claude-code-context)

;; Enable automatic context updates
(claude-code-context-mode 1)

;; Optional: customize the update interval (default: 2 seconds)
(setq claude-code-context-update-interval 3)

;; Optional: customize the context file location
(setq claude-code-context-file "~/.emacs.d/claude-context.txt")
```

### Claude Code Hook Setup

Add this hook configuration to your `~/.claude/settings.json`:

```json
{
  "hooks": {
    "UserPromptSubmit": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "CONTEXT_FILE=\"$HOME/.emacs.d/claude-context.txt\"; if [ -f \"$CONTEXT_FILE\" ]; then echo \"\\n---\\n## Emacs Context\\n\"; cat \"$CONTEXT_FILE\"; echo \"\\n---\"; fi"
          }
        ]
      }
    ]
  }
}
```

See [claude-code-hook-example.json](claude-code-hook-example.json) for a complete example.

## Usage

### Automatic Mode

When `claude-code-context-mode` is enabled, your current buffer context is automatically written to `~/.emacs.d/claude-context.txt` every time Emacs is idle for the configured interval.

### Manual Commands

| Keybinding  | Command                         | Description                                    |
|-------------|---------------------------------|------------------------------------------------|
| `C-c C-l u` | `claude-code-update-context`    | Manually update context                        |
| `C-c C-l d` | `claude-code-add-diagnostics`   | Add flymake diagnostics to context             |
| `C-c C-l c` | `claude-code-clear-context`     | Clear the context file                         |
| `C-c C-l m` | `claude-code-context-mode`      | Toggle automatic context mode                  |

### Workflow Example

1. Open a file in Emacs
2. Position your cursor where you want help
3. Optionally select some code
4. Use `C-c C-l d` to include any linter errors
5. Switch to your terminal and ask Claude Code a question
6. Claude Code will automatically see your Emacs context!

## Context Format

The context file includes:

- **File**: Full path to the current buffer's file
- **Line**: Current line number
- **Column**: Current column number
- **Selection**: Any highlighted text (if region is active)
- **Diagnostics**: Flymake errors/warnings (when using `C-c C-l d`)

Example context:

```
# Emacs Context for Claude Code
# This file is automatically updated by Emacs

File: /Users/username/project/src/main.py
Line: 42, Column: 8
Selection:
```
def process_data(items):
    return [x * 2 for x in items]
```

Flymake Diagnostics:
    45    error  e-f-b-c  undefined variable 'result'
    52  warning  e-f-b-c  unused variable 'temp'
```

## Customization

### Variables

- `claude-code-context-file`: Location of the context file (default: `~/.emacs.d/claude-context.txt`)
- `claude-code-context-update-interval`: Idle time in seconds before updating context (default: 2)

### Custom Keybindings

If you prefer different keybindings:

```elisp
(global-set-key (kbd "C-c u") 'claude-code-update-context)
(global-set-key (kbd "C-c d") 'claude-code-add-diagnostics)
```

## How It Works

1. **Emacs side**: This package writes your current buffer context to a file (`~/.emacs.d/claude-context.txt`)
2. **Claude Code side**: A hook in Claude Code's settings reads this file before every prompt submission
3. **Integration**: Claude Code automatically receives your Emacs context with each query

## Troubleshooting

**Context not appearing in Claude Code:**
- Verify the hook is properly configured in `~/.claude/settings.json`
- Check that the context file exists: `cat ~/.emacs.d/claude-context.txt`
- Make sure `claude-code-context-mode` is enabled in Emacs

**Performance issues:**
- Increase `claude-code-context-update-interval` to reduce update frequency
- Disable automatic mode and use manual commands only

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

GPL-3.0-or-later

## Author

Luke Hoersten <Luke@Hoersten.org>
