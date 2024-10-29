{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    defaultEditor = true;
    extraPackages = with pkgs; [ 
      xclip
      ripgrep
      fd
      (agda.withPackages [ agdaPackages.standard-library ])
      cornelis 
      haskellPackages.haskell-language-server
      clang-tools
      # llvmPackages_15.libstdcxxClang
      llvmPackages.libllvm
      llvmPackages.clangUseLLVM
      pyright
      texlive.combined.scheme-full
      texlab
    ];
    extraLuaConfig = '' 
      vim.g.mapleader     = " "
      vim.opt.incsearch   = true
      vim.opt.tabstop     = 2
      vim.opt.softtabstop = 2
      vim.opt.shiftwidth  = 2
      vim.opt.expandtab   = true
      vim.opt.backup      = false
      vim.opt.scrolloff   = 10
      vim.opt.mouse       = 'a'
      vim.opt.undofile    = true
      vim.opt.signcolumn  = 'yes'
      vim.opt.updatetime  = 250
      -- vim.opt.colorcolumn = '100'
      vim.opt.wrap        = false
    '';
    plugins = with pkgs.vimPlugins; [
      nvim-lspconfig
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      
      {
        plugin = nvim-cmp;
        type = "lua";
        config = '' 
          local cmp = require("cmp")

          cmp.setup({
            snippet = {
              expand = function(args)
                vim.snippet.expand(args.body) 
              end,
            },
            window = {},
            mapping = cmp.mapping.preset.insert({
              ["<C-b>"] = cmp.mapping.scroll_docs(-4),
              ["<C-f>"] = cmp.mapping.scroll_docs(4),
              ["<C-Space>"] = cmp.mapping.complete(),
              ["<C-e>"] = cmp.mapping.abort(),
              ["<CR>"] = cmp.mapping.confirm({ select = true }), 
            }),
            sources = cmp.config.sources({
              { name = "nvim_lsp" },
            }, {
              { name = "buffer" },
            })
          })

          cmp.setup.cmdline({ "/", "?" }, {
            mapping = cmp.mapping.preset.cmdline(),
            sources = {
              { name = "buffer" }
            }
          })

          cmp.setup.cmdline(":", {
            mapping = cmp.mapping.preset.cmdline(),
            sources = cmp.config.sources({
              { name = "path" }
            }, {
              { name = "cmdline" }
            }),
            matching = { disallow_symbol_nonprefix_matching = false }
          })


          vim.keymap.set("n", "<C-y>", vim.diagnostic.open_float)
          vim.keymap.set("n", "K", vim.lsp.buf.hover)

          local capabilities = require("cmp_nvim_lsp").default_capabilities()
          require("lspconfig")["hls"].setup {
            capabilities = capabilities
          }

          require("lspconfig")["pyright"].setup {
            capabilities = capabilities
          }

          require("lspconfig")["clangd"].setup {
            capabilities = capabilities,
            autostart = false
          }

          require("lspconfig")["texlab"].setup {
            capabilities = capabilities,
            autostart = false,
            settings =  {
              build = {
                args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
                executable = "latexmk",
                forwardSearchAfter = false,
                onSave = false
              }
            } 
          }

        ''; 
      }
      {
        plugin = telescope-nvim;
        type = "lua";
        config = ''
          local builtin = require("telescope.builtin")
          vim.keymap.set("n", "<leader>ff", builtin.find_files, {})
          vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})
          vim.keymap.set("n", "<leader>fb", builtin.buffers, {})
          vim.keymap.set("n", "<leader>fh", builtin.help_tags, {})
        '';
      } 
      {
        plugin = cornelis;
        type = "lua";
        config = ''
          vim.g.cornelis_use_global_binary = 1;
          vim.g.cornelis_agda_prefix = "<c-l>";

          local opts = { noremap=true, silent=true }
          vim.cmd([[
            au BufRead,BufNewFile *.agda,*.lagda.* call AgdaFiletype()
            au QuitPre *.agda :CornelisCloseInfoWindows
            function! AgdaFiletype()
              nnoremap <buffer> <leader>l :CornelisLoad<CR>
              nnoremap <buffer> <leader>r :CornelisRefine<CR>
              nnoremap <buffer> <leader>m :CornelisMakeCase<CR>
              nnoremap <buffer> <leader>, :CornelisTypeContext<CR>
              nnoremap <buffer> <leader>. :CornelisTypeContextInfer<CR>
              nnoremap <buffer> <leader>n :CornelisSolve<CR>
              nnoremap <buffer> <leader>a :CornelisAuto<CR>
              nnoremap <buffer> gd        :CornelisGoToDefinition<CR>
              nnoremap <buffer> <leader>k :CornelisPrevGoal<CR>
              nnoremap <buffer> <leader>j :CornelisNextGoal<CR>
              nnoremap <buffer> <C-A>     :CornelisInc<CR>
              nnoremap <buffer> <C-X>     :CornelisDec<CR>
              nnoremap <buffer> <leader>n :CornelisNormalize<CR>
              nnoremap <buffer> <leader>q :CornelisQuestionToMeta<CR>
            endfunction
            au BufWritePost *.agda,*.lagda.* execute "normal! :CornelisLoad\<CR>"
          ]])

        '';
      } 
      {
        plugin = papercolor-theme;
        type = "lua";
        config = '' 
          Colors = {
            GREY   = "#eeeeee",
            YELLOW = "#fdf6e3",
            WHITE  = "#ffffff",
          }

          local background_color = Colors.WHITE

          local colors_light = {
              color07                = {"#111111", "0"},
              color00                = {background_color, ""},
              cursor_fg              = {background_color, ""},
              cursorlinenr_bg        = {background_color, ""},
              linenumber_bg          = {background_color, ""},
              vertsplit_bg           = {background_color, ""},
              todo_bg                = {background_color, ""},
              visual_fg              = {background_color, ""},
              visual_fg              = {background_color, ""},
              tabline_inactive_fg    = {background_color, ""},
              buftabline_active_fg   = {background_color, ""},
              buftabline_inactive_fg = {background_color, ""},
          }

          vim.g.PaperColor_Theme_Options = { 
            theme = {
              default = {
                transparent_background = 0,
                override = colors_light,
                allow_bold = 1
              }
            },
            language = {
              haskell = {
                no_bold_types = 1
              }
            }
          } 
          vim.opt.termguicolors  = true
          vim.opt.background = "light"
          vim.cmd("colorscheme PaperColor")
          vim.cmd("syntax on")
        '';
      }
      {
        plugin = haskell-vim;
        type = "lua";
        config = ''
          vim.g.haskell_enable_quantification   = 0
          vim.g.haskell_enable_recursivedo      = 0
          vim.g.haskell_enable_arrowsyntax      = 0
          vim.g.haskell_enable_pattern_synonyms = 0
          vim.g.haskell_enable_typeroles        = 0
          vim.g.haskell_enable_static_pointers  = 0
          vim.g.haskell_backpack                = 0
          vim.g.haskell_classic_highlighting    = 0
          vim.g.haskell_ident_disable           = 1
        '';
      }
      (nvim-treesitter.withPlugins (p: with p; [ python lua c cpp glsl typst xml]))
      vim-smoothie
      typst-vim
    ];
  };
}
