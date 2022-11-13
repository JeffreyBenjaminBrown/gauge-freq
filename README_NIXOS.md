# To use this in a virtualenv in NixOS

## What I frequently use, in one easy-to-paste brick

nix-shell pip-shell.nix   # Install ornery stuff pip can't handle
python -m venv .venv/     # Make virtualenv if needed
source .venv/bin/activate # Start the virtualenv
PS1="(nix > venv) \W\$ "  # Shorten the prompt
python -m pip install -r requirements.txt
python -m pip install --upgrade pip mypy

## Step by step

### Get it running

`python/requirements.txt` and `pip-shell.nix` work together.
Use them like this:

```
nix-shell pip-shell.nix   # Install ornery stuff pip can't handle
python -m venv .venv/     # Make virtualenv if needed
source .venv/bin/activate # Start the virtualenv
PS1="(nix > venv) \W\$ "  # Shorten the prompt
```


### Install/upgrade Python libraries

After getting it running,
this is necessary the first time,
but usually not thereafter.


#### Install remaining reqs

```
python -m pip install -r python/requirements.txt
```

#### Upgrade some, maybe all

*PITFALL*: Upgrading all might break things,
esp. (according to the warnings) awscli.
So far it seems not to. Here's how to do that:

```
pip list --outdated --format=freeze \
  | awk -F '==' '{print $1}'        \
  | xargs -n1 pip install -U
```

If it does break things,
then instead just update what I know I need to update:

```
python -m pip install --upgrade pip mypy
```


## Exit

```
deactivate        # Exit the virtualenv.
PS1="(nix) \W\$ " # Shorten the prompt
exit              # Exit the Nix shell.
```

Or maybe just Ctrl-D a singl time -- that exits the Nix shell, and I'm assuming closes everything it started.
