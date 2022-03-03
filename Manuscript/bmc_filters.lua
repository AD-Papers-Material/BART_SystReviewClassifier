function Header(el)
  el.classes:insert('unnumbered')
  return el
end

--[[function Image (img)
print("test")

--print(pandoc.utils.stringify(img.c[2]))
--print(img.c[3][1])
print(pandoc.utils.stringify(img.caption))

end--]]

local captions  = pandoc.List()
local tables    = pandoc.List()

function Block (block)

  local to_keep = pandoc.List()

  --for i = 1, #blocks-1, 1 do

    if block.tag == 'RawBlock' then
      local fig_trap = block.text:match("\\begin{figure}")
      local tab_trap = block.text:match("\\begin{table}")

      if fig_trap then
        --print(fig_trap)
        local caption = block.text:match("\\caption%{Figure[^%}]+%}")
        caption = caption:gsub("\\caption%{", ""):gsub("%}$", "")

        captions:insert(caption)
      elseif tab_trap then

        local table = block.text:match("\\begin{table}.*\\end{table}"):gsub("\\begin%{table%}.-%s", "\\begin{table}[H]")

        --print(table)

        tables:insert(pandoc.RawBlock("latex", table))
      else
        to_keep:insert(block)
      end

    elseif block.t == "Para" and #block.c == 1 and block.c[1].t == "Image" then
      --print("Image")
      local caption = pandoc.utils.stringify(block.c[1].caption)

      --print(caption)

      captions:insert(caption)
    else 
      to_keep:insert(block)
    end
  --end

  return to_keep
end


--[[function RawBlock (raw)
  local fig_trap = raw.text:match("\\begin{figure}")
  local tab_trap = raw.text:match("\\begin{table}")
  local to_keep = pandoc.List()

  if fig_trap then
    --print(fig_trap)
    local caption = raw.text:match("\\caption%{Figure[^%}]+%}")
    caption = caption:gsub("\\caption%{", ""):gsub("%}$", "")

    if pandoc.Meta.remove_fig_numbering then
      caption = caption:gsub("\\caption%{", "\\caption%*{")
    end

    captions:insert(caption)
  elseif tab_trap then

    local table = raw.text:match("\\begin{table}.*\\end{table}"):gsub("\\begin%{table%}.-%s", "\\begin{table}[H]")

    --print(table)

    tables:insert(pandoc.RawBlock("latex", table))
  else
    to_keep:insert(raw)
  end

  return to_keep
end--]]


function Pandoc (doc)
  -- for i = 1, #captions, 1 do print(captions[1]) end
  -- append collected captions at the end
  doc.meta.figures = captions --{pandoc.RawBlock('latex', captions)}
  doc.meta.tables = tables
  return doc
end

--[[function Pandoc(doc)
    local hblocks = {}
    for i,el in pairs(doc.blocks) do
        if (el.t == "Div" and el.classes[1] == "handout") or
           (el.t == "BlockQuote") or
           (el.t == "OrderedList" and el.style == "Example") or
           (el.t == "Para" and #el.c == 1 and el.c[1].t == "Image") or
           (el.t == "Header") then
           table.insert(hblocks, el)
        end
    end
    return pandoc.Pandoc(hblocks, doc.meta)
  end--]]

--[[function Pandoc (doc)
  local blocks = doc.blocks
  --local meta = doc.meta
    for i = 1, #blocks-1, 1 do
      print(blocks[i].t)
      for key, value in pairs(blocks[i].c) do
          print('\t', key, value)
      end
      print("\n")
    
  end
    return doc
  end--]]

--[[function Pandoc (doc)
  local blocks = doc.blocks
  local meta = doc.meta
    for i = 1, #blocks-1, 1 do
      if (blocks[i].t == 'Table') then
        print(pandoc.utils.stringify(blocks[i]))
        tables:insert(blocks[i].c)
      end
      print("\n")
    
  end
    return doc
  end--]]

--[[function Para(elem)
    if #elem.content == 1 and elem.content[1].t == "Image" then
      local img = elem.content[1]

      print(img)  

        -- now we need to create a header from the metadata
        local title=pandoc.utils.stringify(doc.meta.title) or "Title has not been set"
         local newHeader=pandoc.Header(1, {pandoc.Str(title)})

        new_el = {pandoc.RawInline('latex', '\\begin{figure}[h!]\\caption{'), img.citation, pandoc.RawInline('latex', '}\\end{figure}')}
        table.insert(doc.blocks, 1, new_el) 
        return doc.blocks
    end
end--]]