// export type OrgFile = OrgTree[];

export type OrgTree = {
  heading: string;
  preamble: string;
  todoState: OrgTodo | null;
  children: OrgTree[];
};

export type OrgTodo = "TODO" | "IN_PROGRESS" | "DONE";

export type Todos = Record<string, OrgFile>

export type OrgFile = {
  orgDoc: OrgDoc;
  orgMeta: OrgMeta;
}

export type OrgMeta = {
  title?: string;
}

export type OrgDoc = {
  docBlocks: Block[];
  docSections: Section[];
}

export type Section = {
  sectionTodo: Todo | null;
  sectionHeading: Words[];
  sectionDoc: OrgDoc;
}

export type Words =
  { tag: "Plain", contents: string }
  | any;

export type Block = object;

export type Todo = "TODO" | "DONE";
