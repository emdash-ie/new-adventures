export type OrgFile = OrgTree[];

export type OrgTree = {
  heading: string;
  preamble: string;
  todoState: OrgTodo | null;
  children: OrgTree[];
};

export type OrgTodo = "TODO" | "IN_PROGRESS" | "DONE";
