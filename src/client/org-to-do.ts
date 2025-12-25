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
  docBlocks: object;
  docSections: object;
}
