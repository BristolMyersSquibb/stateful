# Sample Data Files

This directory contains sample RTF files and their corresponding JSON outputs for testing and demonstration.

## Directory Structure

- **`rtf_inputs/`** - Input RTF files from clinical trial tables
- **`json_outputs/`** - Parsed JSON outputs in ARD format
  - `*_ard.json` - Legacy ARD format outputs
  - `*_state_parser.json` - New state parser outputs with improved nested header handling

## File Naming Convention

Files follow the pattern: `rt-[domain]-[description].rtf`

- `rt-dm-*` - Demographics tables
- `rt-ae-*` - Adverse Events tables  
- `rt-saf-*` - Safety tables
- `rt-ds-*` - Disposition tables
- `rt-ef-*` - Efficacy tables

## Latest Improvements

The state parser JSON files include:
- Fixed nested header parsing with proper `group1`/`group1_level` and `group2`/`group2_level` structure
- Improved footnote detection to prevent footnotes from being parsed as table data
- Enhanced hierarchical variable handling for nested row structures